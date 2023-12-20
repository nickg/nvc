//
//  Copyright (C) 2011-2022  Nick Gasson
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#include "util.h"
#include "common.h"
#include "diag.h"
#include "fbuf.h"
#include "hash.h"
#include "ident.h"
#include "lib.h"
#include "object.h"
#include "option.h"
#include "tree.h"
#include "vlog/vlog-node.h"

#include <assert.h>
#include <limits.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <inttypes.h>
#include <unistd.h>
#include <dirent.h>
#include <errno.h>
#include <time.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/time.h>

typedef struct _search_path search_path_t;
typedef struct _lib_index   lib_index_t;
typedef struct _lib_list    lib_list_t;
typedef struct _lib_unit    lib_unit_t;

#define INDEX_FILE_MAGIC 0x55225511

struct _lib_unit {
   object_t     *object;
   ident_t       name;
   tree_kind_t   kind;
   bool          dirty;
   bool          error;
   uint64_t      mtime;
   lib_unit_t   *next;
};

struct _lib_index {
   ident_t      name;
   tree_kind_t  kind;
   lib_index_t *next;
};

struct _lib {
   char         *path;
   ident_t       name;
   hash_t       *lookup;
   lib_unit_t   *units;
   lib_index_t  *index;
   uint64_t      index_mtime;
   off_t         index_size;
   int           lock_fd;
   bool          readonly;
};

struct _lib_list {
   lib_t            item;
   lib_list_t      *next;
   vhdl_standard_t  standard;
};

struct _search_path {
   search_path_t *next;
   const char    *path;
};

static lib_t          work = NULL;
static lib_list_t    *loaded = NULL;
static search_path_t *search_paths = NULL;

static text_buf_t *lib_file_path(lib_t lib, const char *name);

static const char *standard_suffix(vhdl_standard_t std)
{
   static const char *ext[] = {
      "87", "93", "00", "02", "08", "19"
   };

   assert(std < ARRAY_LEN(ext));
   return ext[std];
}

static ident_t upcase_name(const char *name)
{
   char *name_copy LOCAL = xstrdup(name);

   char *name_up = name_copy;
   for (char *p = name_up; *p != '\0'; p++)
      *p = toupper_iso88591(*p);

   char *last_dot = strrchr(name_up, '.');
   if (last_dot != NULL)
      *last_dot = '\0';

   return ident_new(name_up);
}

static void lib_add_to_index(lib_t lib, ident_t name, tree_kind_t kind)
{
   // Keep the index in sorted order to make library builds reproducible
   lib_index_t **it;
   for (it = &(lib->index);
        *it != NULL && ident_compare((*it)->name, name) < 0;
        it = &((*it)->next))
      ;

   if (*it != NULL && (*it)->name == name) {
      // Already in the index
      (*it)->kind = kind;
   }
   else {
      lib_index_t *new = xmalloc(sizeof(lib_index_t));
      new->name = name;
      new->kind = kind;
      new->next = *it;

      *it = new;
   }
}

static void lib_read_index(lib_t lib)
{
   fbuf_t *f = lib_fbuf_open(lib, "_index", FBUF_IN, FBUF_CS_NONE);
   if (f != NULL) {
      file_info_t info;
      if (!get_handle_info(fbuf_file_handle(f), &info))
         fatal_errno("%s", fbuf_file_name(f));

      const uint32_t magic = read_u32(f);
      if (magic != INDEX_FILE_MAGIC) {
         warnf("ignoring library index %s from an old version of " PACKAGE,
               fbuf_file_name(f));
         return;
      }

      lib->index_mtime = info.mtime;
      lib->index_size  = info.size;

      ident_rd_ctx_t ictx = ident_read_begin(f);
      lib_index_t **insert = &(lib->index);

      const int entries = read_u32(f);
      for (int i = 0; i < entries; i++) {
         ident_t name = ident_read(ictx);
         tree_kind_t kind = read_u16(f);
         assert(kind <= T_LAST_TREE_KIND);

         while (*insert && ident_compare((*insert)->name, name) < 0)
            insert = &((*insert)->next);

         if (*insert && (*insert)->name == name) {
            (*insert)->kind = kind;
            insert = &((*insert)->next);
         }
         else {
            lib_index_t *new = xmalloc(sizeof(lib_index_t));
            new->name = name;
            new->kind = kind;
            new->next = *insert;

            *insert = new;
            insert = &(new->next);
         }
      }

      ident_read_end(ictx);
      fbuf_close(f, NULL);
   }
}

static lib_t lib_init(const char *name, const char *rpath, int lock_fd)
{
   lib_t l = xcalloc(sizeof(struct _lib));
   l->name     = upcase_name(name);
   l->index    = NULL;
   l->lock_fd  = lock_fd;
   l->readonly = false;
   l->lookup   = hash_new(128);

   char abspath[PATH_MAX];
   if (rpath == NULL)
      l->path = NULL;
   else if (realpath(rpath, abspath) == NULL)
      l->path = xstrdup(rpath);
   else
      l->path = xstrdup(abspath);

   lib_list_t *el = xcalloc(sizeof(lib_list_t));
   el->item     = l;
   el->next     = loaded;
   el->standard = standard();
   loaded = el;

   if (opt_get_verbose(OPT_LIB_VERBOSE, name))
      debugf("library %s at %s", istr(l->name), l->path);

   if (l->lock_fd == -1 && rpath != NULL) {
      LOCAL_TEXT_BUF lock_path = lib_file_path(l, "_NVC_LIB");

      // Try to open the lock file read-write as this is required for
      // exlusive locking on some NFS implementations
      int mode = O_RDWR;
      if (access(tb_get(lock_path), mode) != 0) {
         if (errno == EACCES || errno == EPERM || errno == EROFS) {
            mode = O_RDONLY;
            l->readonly = true;
         }
         else
            fatal_errno("access: %s", tb_get(lock_path));
      }

      if ((l->lock_fd = open(tb_get(lock_path), mode)) < 0)
         fatal_errno("open: %s", tb_get(lock_path));

      file_read_lock(l->lock_fd);
   }

   lib_read_index(l);

   if (l->lock_fd != -1)
      file_unlock(l->lock_fd);

   return l;
}

static lib_index_t *lib_find_in_index(lib_t lib, ident_t name)
{
   lib_index_t *it;
   for (it = lib->index;
        (it != NULL) && (it->name != name);
        it = it->next)
      ;

   return it;
}

static lib_unit_t *lib_put_aux(lib_t lib, object_t *object, bool dirty,
                               bool error, timestamp_t mtime, vcode_unit_t vu)
{
   assert(lib != NULL);

   tree_kind_t kind = T_LAST_TREE_KIND;
   ident_t name;
   tree_t tree;
   vlog_node_t vlog;
   if ((tree = tree_from_object(object))) {
      name = tree_ident(tree);
      kind = tree_kind(tree);
   }
   else if ((vlog = vlog_from_object(object)))
      name = vlog_ident(vlog);
   else
      fatal_trace("unexpected object class in lib_put_aux");

   assert(ident_until(name, '.') == lib->name);

   bool fresh = false;
   lib_unit_t *where = hash_get(lib->lookup, name);
   if (where == NULL) {
      where = xcalloc(sizeof(lib_unit_t));
      fresh = true;
   }
   else
      hash_delete(lib->lookup, object);

   where->object = object;
   where->name   = name;
   where->dirty  = dirty;
   where->error  = error;
   where->mtime  = mtime;
   where->kind   = kind;

   if (fresh) {
      lib_unit_t **it;
      for (it = &(lib->units); *it; it = &(*it)->next) {
         assert((*it)->object != object);
         assert((*it)->name != name);
      }
      *it = where;
   }

   lib_add_to_index(lib, name, where->kind);

   hash_put(lib->lookup, name, where);
   hash_put(lib->lookup, object, where);

   return where;
}

static lib_t lib_open_at(const char *name, const char *path)
{
   if (access(path, F_OK) < 0)
      return NULL;

   char *marker LOCAL = xasprintf("%s" DIR_SEP "_NVC_LIB", path);
   if (access(marker, F_OK) < 0)
      return NULL;

   return lib_init(name, path, -1);
}

static lib_t lib_find_at(const char *name, const char *path)
{
   DIR *d = opendir(path);
   if (d == NULL)
      return NULL;

   char *best LOCAL = NULL;
   const char *std_suffix = standard_suffix(standard());
   const size_t namelen = strlen(name);

   struct dirent *e;
   while ((e = readdir(d))) {
      if (!isalpha(e->d_name[0]))
         continue;

      const char *dot = strchr(e->d_name, '.');
      if (dot != NULL) {
         if (namelen != dot - e->d_name)
            continue;
         else if (strncasecmp(name, e->d_name, namelen) != 0)
            continue;
         else if (strcmp(dot + 1, std_suffix) != 0)
            continue;
      }
      else {
         if (strcasecmp(name, e->d_name) != 0)
            continue;
         else if (best != NULL)
            continue;
      }

      free(best);
      best = xstrdup(e->d_name);
   }

   closedir(d);

   if (best == NULL)
      return NULL;

   LOCAL_TEXT_BUF dir = tb_new();
   tb_cat(dir, path);
   tb_cat(dir, DIR_SEP);
   tb_cat(dir, best);

   char *marker LOCAL = xasprintf("%s" DIR_SEP "_NVC_LIB", tb_get(dir));
   if (access(marker, F_OK) < 0)
      return NULL;

   return lib_init(name, tb_get(dir), -1);
}

static text_buf_t *lib_file_path(lib_t lib, const char *name)
{
   text_buf_t *tb = tb_new();
   tb_printf(tb, "%s" DIR_SEP "%s", lib->path, name);
   return tb;
}

lib_t lib_loaded(ident_t name_i)
{
   if (name_i == well_known(W_WORK) && work != NULL)
      return work;

   for (lib_list_t *it = loaded; it != NULL; it = it->next) {
      if (lib_name(it->item) == name_i && it->standard == standard())
         return it->item;
   }

   return NULL;
}

lib_t lib_new(const char *spec)
{
   char *copy LOCAL = xstrdup(spec);

#ifdef __MINGW32__
   char *split = strchr(copy, ';');
   if (split == NULL) {
      split = strchr(copy, ':');

      // Ignore a leading drive letter in the path
      if (split == copy + 1 && (copy[2] == '/' || copy[2] == '\\'))
         split = NULL;
   }
#else
   char *split = strchr(copy, ':');
#endif

   const char *path, *name, *search = NULL;
   if (split == NULL) {
      // No colon in the argument means search in the given directory
      char *slash = strrchr(copy, *DIR_SEP) ?: strrchr(copy, '/');
      if (slash == NULL) {
         name = path = copy;
         search = ".";
      }
      else {
         *slash = '\0';
         name = slash + 1;
         path = spec;
         search = copy;
      }
   }
   else {
      // The string after the colon specifies the exact library path
      *split = '\0';
      name = copy;
      path = split + 1;
   }

   if (*name == '\0')
      fatal("library name cannot be empty");

   const char *last_dot = strrchr(name, '.');
   if (last_dot != NULL) {
      const char *ext = standard_suffix(standard());
      if (strcmp(last_dot + 1, ext) != 0)
         fatal("library directory suffix must be '%s' for this standard", ext);
   }

   for (const char *p = name; *p && p != last_dot; p++) {
      if (!isalnum_iso88591(*p) && (*p != '_'))
         fatal("invalid character '%c' in library name", *p);
   }

   ident_t name_i = upcase_name(name);
   lib_t lib = lib_loaded(name_i);
   if (lib != NULL)
      return lib;
   else if (search != NULL && (lib = lib_find_at(name, search)) != NULL)
      return lib;
   else if (search == NULL && (lib = lib_open_at(name, path)) != NULL)
      return lib;

   char *lockf LOCAL = xasprintf("%s" DIR_SEP "%s", path, "_NVC_LIB");

   bool existing = false;
   file_info_t dir_info;
   if (get_file_info(path, &dir_info)) {
      if (dir_info.type == FILE_DIR) {
         file_info_t lockf_info;
         existing = !get_file_info(lockf, &lockf_info);
      }
      else
         fatal("path %s already exists and is not a directory", path);
   }

   make_dir(path);

   int fd = open(lockf, O_CREAT | O_EXCL | O_RDWR, 0777);
   if (fd < 0) {
      // If errno is EEXIST we raced with another process to create the
      // lock file.  Calling into lib_init with fd as -1 will cause it
      // to be opened again without O_CREAT.
      if (errno != EEXIST)
         fatal_errno("lib_new: %s", lockf);
   }
   else {
      file_write_lock(fd);

      if (existing) {
         // We cannot do this check above as we may be racing with another
         // process trying to create the library which has already made the
         // directory but not yet created the lock file
         warnf("directory %s already exists and is not an NVC library",
               path);
      }

      const char *marker = PACKAGE_STRING "\n";
      if (write(fd, marker, strlen(marker)) < 0)
         fatal_errno("write: %s", path);
   }

   return lib_init(name, path, fd);
}

lib_t lib_tmp(const char *name)
{
   // For unit tests, avoids creating files
   return lib_init(name, NULL, -1);
}

static void push_path(const char *path)
{
   for (search_path_t *it = search_paths; it != NULL; it = it->next) {
      if (strcmp(it->path, path) == 0)
         return;
   }

   search_path_t *s = xmalloc(sizeof(search_path_t));
   s->next = search_paths;
   s->path = strdup(path);

   search_paths = s;
}

static void lib_default_search_paths(void)
{
#ifdef ENABLE_DEFAULT_PATHS
   if (search_paths == NULL) {
      LOCAL_TEXT_BUF tb = tb_new();
      get_lib_dir(tb);
      push_path(tb_get(tb));

      const char *home_env = getenv("HOME");
      if (home_env) {
         char *LOCAL path = xasprintf("%s/.%s/lib", home_env, PACKAGE);
         push_path(path);
      }

      char *LOCAL env_copy = NULL;
      const char *libpath_env = getenv("NVC_LIBPATH");
      if (libpath_env) {
         env_copy = strdup(libpath_env);

         char *path_tok = strtok(env_copy, ":");
         do {
            push_path(path_tok);
         } while ((path_tok = strtok(NULL, ":")));
      }
   }
#endif
}

void lib_add_search_path(const char *path)
{
   lib_default_search_paths();
   push_path(path);
}

void lib_add_map(const char *name, const char *path)
{
   lib_t lib = lib_open_at(name, path);
   if (lib == NULL)
      warnf("library %s not found at %s", name, path);
}

void lib_print_search_paths(text_buf_t *tb)
{
   lib_default_search_paths();

   for (search_path_t *it = search_paths; it != NULL; it = it->next)
      tb_printf(tb, "\n  %s", it->path);
}

void lib_search_paths_to_diag(diag_t *d)
{
   lib_default_search_paths();

   LOCAL_TEXT_BUF tb = tb_new();
   tb_printf(tb, "library search path contains: ");

   for (search_path_t *it = search_paths; it != NULL; it = it->next)
      tb_printf(tb, "%s%s", it != search_paths ? ", " : "", it->path);

   diag_hint(d, NULL, "%s", tb_get(tb));
   diag_hint(d, NULL, "add additional directories to the search path with "
             "the $bold$-L$$ option");
}

lib_t lib_find(ident_t name_i)
{
   lib_t lib = lib_loaded(name_i);
   if (lib != NULL)
      return lib;

   lib_default_search_paths();

   const char *name_str = istr(name_i);
   for (search_path_t *it = search_paths; it != NULL; it = it->next) {
      if ((lib = lib_find_at(name_str, it->path)))
         break;
   }

   return lib;
}

lib_t lib_require(ident_t name)
{
   lib_t lib = lib_find(name);
   if (lib == NULL) {
      diag_t *d = diag_new(DIAG_FATAL, NULL);
      diag_suppress(d, false);
      diag_printf(d, "required library %s not found", istr(name));
      lib_search_paths_to_diag(d);
      diag_emit(d);
      fatal_exit(EXIT_FAILURE);
   }

   return lib;
}

FILE *lib_fopen(lib_t lib, const char *name, const char *mode)
{
   assert(lib != NULL);
   LOCAL_TEXT_BUF path = lib_file_path(lib, name);
   return fopen(tb_get(path), mode);
}

fbuf_t *lib_fbuf_open(lib_t lib, const char *name,
                      fbuf_mode_t mode, fbuf_cs_t csum)
{
   assert(lib != NULL);
   if (lib->path == NULL)
      return NULL;   // Temporary library for unit test
   else {
      LOCAL_TEXT_BUF path = lib_file_path(lib, name);
      return fbuf_open(tb_get(path), mode, csum);
   }
}

void lib_free(lib_t lib)
{
   assert(lib != NULL);
   assert(lib != work);

   if (lib->lock_fd != -1)
      close(lib->lock_fd);

   for (lib_list_t **it = &loaded; *it; it = &((*it)->next)) {
      if ((*it)->item == lib) {
         lib_list_t *tmp = *it;
         *it = (*it)->next;
         free(tmp);
         break;
      }
   }

   while (lib->index) {
      lib_index_t *tmp = lib->index->next;
      free(lib->index);
      lib->index = tmp;
   }

   for (lib_unit_t *lu = lib->units, *tmp; lu; lu = tmp) {
      tmp = lu->next;
      free(lu);
   }
   hash_free(lib->lookup);

   free(lib->path);
   free(lib);
}

void lib_destroy(lib_t lib)
{
   // This is convenience function for testing: remove all
   // files associated with a library

   assert(lib != NULL);

   close(lib->lock_fd);
   lib->lock_fd = -1;

   DIR *d = opendir(lib->path);
   if (d == NULL) {
      perror("opendir");
      return;
   }

   char buf[PATH_MAX];
   struct dirent *e;
   while ((e = readdir(d))) {
      if (e->d_name[0] != '.') {
         checked_sprintf(buf, sizeof(buf), "%s" DIR_SEP "%s",
                         lib->path, e->d_name);
         if (unlink(buf) < 0)
            fatal_errno("unlink");
      }
   }

   closedir(d);

   if (rmdir(lib->path) < 0)
      fatal_errno("rmdir");
}

lib_t lib_work(void)
{
   assert(work != NULL);
   return work;
}

void lib_set_work(lib_t lib)
{
   work = lib;
}

const char *lib_path(lib_t lib)
{
   assert(lib != NULL);
   return lib->path;
}

void lib_put(lib_t lib, tree_t unit)
{
   object_t *obj = tree_to_object(unit);
   lib_put_aux(lib, obj, true, false, get_real_time(), NULL);
}

void lib_put_vlog(lib_t lib, vlog_node_t module)
{
   assert(vlog_kind(module) == V_MODULE);
   object_t *obj = vlog_to_object(module);
   lib_put_aux(lib, obj, true, false, get_real_time(), NULL);
}

void lib_put_error(lib_t lib, tree_t unit)
{
   object_t *obj = tree_to_object(unit);
   lib_put_aux(lib, obj, true, true, get_real_time(), NULL);
}

static void lib_encode_file_name(ident_t id, text_buf_t *tb)
{
   // Encode extended identifiers in file names using the hexadecimal
   // value of the ISO-8859-1 code points to avoid illegal characters
   // and case sensitivity issues on Windows and macOS

   bool esc = false;
   for (const char *p = istr(id); *p; p++) {
      if (*p == '\\') {
         esc = !esc;
         tb_append(tb, '+');
      }
      else if (esc)
         tb_printf(tb, "%02x", *p);
      else
         tb_append(tb, *p);
   }
}

static lib_unit_t *lib_read_unit(lib_t lib, ident_t id)
{
   LOCAL_TEXT_BUF tb = tb_new();
   lib_encode_file_name(id, tb);

   fbuf_t *f = lib_fbuf_open(lib, tb_get(tb), FBUF_IN, FBUF_CS_ADLER32);
   if (f == NULL)
      return NULL;

   file_info_t info;
   if (!get_handle_info(fbuf_file_handle(f), &info))
      fatal_errno("%s", tb_get(tb));

   ident_rd_ctx_t ident_ctx = ident_read_begin(f);
   loc_rd_ctx_t *loc_ctx = loc_read_begin(f);

   vcode_unit_t vu = NULL;
   object_t *obj = NULL;

   char tag;
   while ((tag = read_u8(f))) {
      switch (tag) {
      case 'T':
         obj = object_read(f, lib_load_handler, ident_ctx, loc_ctx);
         break;
      case 'V':
         // Ignore it (remove after 1.12 release)
         break;
      default:
         fatal_trace("unhandled tag %c in %s", tag, tb_get(tb));
      }
   }

   loc_read_end(loc_ctx);
   ident_read_end(ident_ctx);

   uint32_t checksum;
   fbuf_close(f, &checksum);

   if (obj == NULL)
      fatal_trace("%s did not contain a HDL design unit", tb_get(tb));

   arena_set_checksum(object_arena(obj), checksum);

   return lib_put_aux(lib, obj, false, false, info.mtime, vu);
}

static lib_unit_t *lib_get_aux(lib_t lib, ident_t ident)
{
   assert(lib != NULL);

   // Handle aliased library names and names without the library
   ident_t uname = ident, lname = ident_walk_selected(&uname);
   if (uname == NULL)
      ident = ident_prefix(lib->name, lname, '.');
   else if (lname != lib->name)
      ident = ident_prefix(lib->name, uname, '.');

   lib_unit_t *lu = hash_get(lib->lookup, ident);
   if (lu != NULL)
      return lu;

   if (lib->path == NULL)   // Temporary library
      return NULL;

   assert(lib->lock_fd != -1);   // Should not be called in unit tests
   file_read_lock(lib->lock_fd);

   // Otherwise search in the filesystem if not in the cache
   lu = lib_read_unit(lib, ident);

   file_unlock(lib->lock_fd);

   if (lu == NULL && lib_find_in_index(lib, ident) != NULL)
      fatal("library %s corrupt: unit %s present in index but missing "
            "on disk", istr(lib->name), istr(ident));

   if (lu != NULL && !opt_get_int(OPT_IGNORE_TIME)) {
      bool stale = false;
      file_info_t info;
      const char *file = loc_file_str(&(lu->object->loc));
      if (get_file_info(file, &info))
         stale = (lu->mtime < info.mtime);

      if (stale) {
         diag_t *d = diag_new(DIAG_WARN, NULL);
         diag_printf(d, "design unit %s is older than its source file "
                     "%s and should be reanalysed", istr(ident), file);
         diag_hint(d, NULL, "you can use the $bold$--ignore-time$$ option "
                   "to skip this check");
         diag_emit(d);
      }
   }

   return lu;
}

static void lib_ensure_writable(lib_t lib)
{
   if (lib->readonly)
      fatal("cannot write to read-only library %s", istr(lib->name));
}

object_t *lib_get_generic(lib_t lib, ident_t ident)
{
   lib_unit_t *lu = lib_get_aux(lib, ident);
   if (lu != NULL) {
      if (lu->error)
         fatal_at(&lu->object->loc, "design unit %s was analysed with errors",
                  istr(lu->name));
      else
         return lu->object;
   }
   else
      return NULL;
}

tree_t lib_get(lib_t lib, ident_t ident)
{
   object_t *obj = lib_get_generic(lib, ident);
   if (obj == NULL)
      return NULL;

   tree_t tree = tree_from_object(obj);
   if (tree == NULL)
      fatal_at(&obj->loc, "%s is not a VHDL design unit", istr(ident));

   return tree;
}

tree_t lib_get_allow_error(lib_t lib, ident_t ident, bool *error)
{
   lib_unit_t *lu = lib_get_aux(lib, ident);
   if (lu != NULL) {
      *error = lu->error;

      tree_t tree = tree_from_object(lu->object);
      if (tree == NULL)
         fatal_at(&lu->object->loc, "%s is not a VHDL design unit",
                  istr(lu->name));

      return tree;
   }
   else {
      *error = false;
      return NULL;
   }
}

object_t *lib_load_handler(ident_t qual)
{
   ident_t lname = ident_until(qual, '.');
   if (lname == NULL)
      return NULL;

   lib_t lib = lib_find(lname);
   if (lib == NULL)
      return NULL;

   return lib_get_generic(lib, qual);
}

tree_t lib_get_qualified(ident_t qual)
{
   object_t *obj = lib_load_handler(qual);
   if (obj == NULL)
      return NULL;

   tree_t tree = tree_from_object(obj);
   if (tree == NULL)
      fatal_at(&obj->loc, "%s is not a VHDL design unit", istr(qual));

   return tree;
}

timestamp_t lib_get_mtime(lib_t lib, ident_t ident)
{
   lib_unit_t *lu = hash_get(lib->lookup, ident);
   if (lu != NULL)
      return lu->mtime;

   LOCAL_TEXT_BUF path = lib_file_path(lib, istr(ident));

   file_info_t info;
   if (get_file_info(tb_get(path), &info))
      return info.mtime;

   return 0;
}

bool lib_had_errors(lib_t lib, ident_t ident)
{
   lib_unit_t *lu = lib_get_aux(lib, ident);
   if (lu != NULL)
      return lu->error;

   return false;
}

ident_t lib_name(lib_t lib)
{
   assert(lib != NULL);
   return lib->name;
}

static void lib_save_unit(lib_t lib, lib_unit_t *unit)
{
   LOCAL_TEXT_BUF tb = tb_new();
   lib_encode_file_name(unit->name, tb);

   fbuf_t *f = lib_fbuf_open(lib, tb_get(tb), FBUF_OUT, FBUF_CS_ADLER32);
   if (f == NULL)
      fatal("failed to create %s in library %s", tb_get(tb), istr(lib->name));

   write_u8('T', f);

   ident_wr_ctx_t ident_ctx = ident_write_begin(f);
   loc_wr_ctx_t *loc_ctx = loc_write_begin(f);

   object_arena_t *arena = object_arena(unit->object);

   object_write(unit->object, f, ident_ctx, loc_ctx);

   write_u8('\0', f);

   loc_write_end(loc_ctx);
   ident_write_end(ident_ctx);

   uint32_t checksum;
   fbuf_close(f, &checksum);

   arena_set_checksum(arena, checksum);

   assert(unit->dirty);
   unit->dirty = false;
}

void lib_save(lib_t lib)
{
   assert(lib != NULL);

   assert(lib->lock_fd != -1);   // Should not be called in unit tests
   lib_ensure_writable(lib);
   file_write_lock(lib->lock_fd);

   freeze_global_arena();

   for (lib_unit_t *lu = lib->units; lu; lu = lu->next) {
      if (lu->dirty) {
         if (lu->error)
            fatal_trace("attempting to save unit %s with errors",
                        istr(lu->name));
         else
            lib_save_unit(lib, lu);
      }
   }

   LOCAL_TEXT_BUF index_path = lib_file_path(lib, "_index");
   file_info_t info;
   if (get_file_info(tb_get(index_path), &info)) {
      if (info.mtime != lib->index_mtime || info.size != lib->index_size) {
         // Library was updated concurrently: re-read the index while we
         // have the lock
         lib_read_index(lib);
      }
   }

   int index_sz = lib_index_size(lib);

   fbuf_t *f = lib_fbuf_open(lib, "_index", FBUF_OUT, FBUF_CS_NONE);
   if (f == NULL)
      fatal_errno("failed to create library %s index", istr(lib->name));

   write_u32(INDEX_FILE_MAGIC, f);

   ident_wr_ctx_t ictx = ident_write_begin(f);

   write_u32(index_sz, f);
   for (lib_index_t *it = lib->index; it != NULL; it = it->next) {
      ident_write(it->name, ictx);
      write_u16(it->kind, f);
   }

   ident_write_end(ictx);
   fbuf_close(f, NULL);

   if (!get_file_info(tb_get(index_path), &info))
      fatal_errno("%s", tb_get(index_path));

   lib->index_mtime = info.mtime;
   lib->index_size  = info.size;

   file_unlock(lib->lock_fd);
}

int lib_index_kind(lib_t lib, ident_t ident)
{
   lib_index_t *it = lib_find_in_index(lib, ident);
   return it != NULL ? it->kind : T_LAST_TREE_KIND;
}

void lib_walk_index(lib_t lib, lib_index_fn_t fn, void *context)
{
   assert(lib != NULL);

   lib_index_t *it;
   for (it = lib->index; it != NULL; it = it->next)
      (*fn)(lib, it->name, it->kind, context);
}

void lib_for_all(lib_walk_fn_t fn, void *ctx)
{
   for (lib_list_t *it = loaded; it != NULL; it = it->next) {
      if (!(*fn)(it->item, ctx))
         break;
   }
}

unsigned lib_index_size(lib_t lib)
{
   assert(lib != NULL);

   unsigned n = 0;
   for (lib_index_t *it = lib->index; it != NULL; it = it->next)
      n++;

   return n;
}

void lib_realpath(lib_t lib, const char *name, char *buf, size_t buflen)
{
   assert(lib != NULL);

   if (lib->path == NULL)
      checked_sprintf(buf, buflen, "." DIR_SEP "%s", name);
   else if (name)
      checked_sprintf(buf, buflen, "%s" DIR_SEP "%s", lib->path, name);
   else
      checked_sprintf(buf, buflen, "%s", lib->path);
}

void lib_delete(lib_t lib, const char *name)
{
   assert(lib != NULL);
   LOCAL_TEXT_BUF path = lib_file_path(lib, name);
   if (remove(tb_get(path)) != 0 && errno != ENOENT)
      fatal_errno("remove: %s", name);
}
