//
//  Copyright (C) 2011-2021  Nick Gasson
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
#include "lib.h"
#include "tree.h"
#include "common.h"
#include "loc.h"
#include "vcode.h"

#include <assert.h>
#include <limits.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>
#include <dirent.h>
#include <errno.h>
#include <time.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/time.h>

typedef struct search_path search_path_t;
typedef struct lib_unit    lib_unit_t;
typedef struct lib_index   lib_index_t;
typedef struct lib_list    lib_list_t;
typedef struct lib_map     lib_map_t;

struct lib_unit {
   tree_t        top;
   tree_kind_t   kind;
   tree_rd_ctx_t read_ctx;
   bool          dirty;
   lib_mtime_t   mtime;
};

struct lib_index {
   ident_t      name;
   tree_kind_t  kind;
   lib_index_t *next;
};

struct lib {
   char         path[PATH_MAX];
   ident_t      name;
   unsigned     n_units;
   unsigned     units_alloc;
   lib_unit_t  *units;
   lib_index_t *index;
   lib_mtime_t  index_mtime;
   off_t        index_size;
   int          lock_fd;
   bool         readonly;
};

struct lib_list {
   lib_t            item;
   lib_list_t      *next;
   vhdl_standard_t  standard;
};

struct search_path {
   search_path_t *next;
   const char    *path;
};

static lib_t          work = NULL;
static lib_list_t    *loaded = NULL;
static search_path_t *search_paths = NULL;

static text_buf_t *lib_file_path(lib_t lib, const char *name);
static lib_mtime_t lib_stat_mtime(struct stat *st);

static const char *standard_suffix(vhdl_standard_t std)
{
   static const char *ext[] = {
      "87", "93", "00", "02", "08"
   };

   assert(std < ARRAY_LEN(ext));
   return ext[std];
}

static ident_t upcase_name(const char *name)
{
   char *name_copy LOCAL = xstrdup(name);

   char *last_slash = strrchr(name_copy, '/');
   while ((last_slash != NULL) && (*(last_slash + 1) == '\0')) {
      *last_slash = '\0';
      last_slash = strrchr(name_copy, '/');
   }

   char *name_up = (last_slash != NULL) ? last_slash + 1 : name_copy;
   for (char *p = name_up; *p != '\0'; p++)
      *p = toupper((int)*p);

   char *last_dot = strrchr(name_up, '.');
   if (last_dot != NULL)
      *last_dot = '\0';

   if (*name_up == '\0')
      fatal("invalid library name %s", name);

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
   fbuf_t *f = lib_fbuf_open(lib, "_index", FBUF_IN);
   if (f != NULL) {
      struct stat st;
      if (stat(fbuf_file_name(f), &st) < 0)
         fatal_errno("%s", fbuf_file_name(f));

      lib->index_mtime = lib_stat_mtime(&st);
      lib->index_size  = st.st_size;

      ident_rd_ctx_t ictx = ident_read_begin(f);
      lib_index_t **insert = &(lib->index);

      const int entries = read_u32(f);
      for (int i = 0; i < entries; i++) {
         ident_t name = ident_read(ictx);
         tree_kind_t kind = read_u16(f);
         assert(kind < T_LAST_TREE_KIND);

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
      fbuf_close(f);
   }
}

static lib_t lib_init(const char *name, const char *rpath, int lock_fd)
{
   struct lib *l = xcalloc(sizeof(struct lib));
   l->n_units  = 0;
   l->units    = NULL;
   l->name     = upcase_name(name);
   l->index    = NULL;
   l->lock_fd  = lock_fd;
   l->readonly = false;

   if (rpath == NULL)
      l->path[0] = '\0';
   else if (realpath(rpath, l->path) == NULL)
      checked_sprintf(l->path, PATH_MAX, "%s", rpath);

   lib_list_t *el = xcalloc(sizeof(lib_list_t));
   el->item     = l;
   el->next     = loaded;
   el->standard = standard();
   loaded = el;

   if (l->lock_fd == -1 && rpath != NULL) {
      LOCAL_TEXT_BUF lock_path = lib_file_path(l, "_NVC_LIB");

      // Try to open the lock file read-write as this is required for
      // exlusive locking on some NFS implementations
      int mode = O_RDWR;
      if (access(tb_get(lock_path), mode) != 0) {
         if (errno == EACCES || errno == EPERM) {
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

static lib_unit_t *lib_put_aux(lib_t lib, tree_t unit,
                               tree_rd_ctx_t ctx, bool dirty,
                               lib_mtime_t mtime)
{
   assert(lib != NULL);
   assert(unit != NULL);

   lib_unit_t *where = NULL;
   ident_t name = tree_ident(unit);

   for (unsigned i = 0; (where == NULL) && (i < lib->n_units); i++) {
      if (tree_ident(lib->units[i].top) == tree_ident(unit))
         where = &(lib->units[i]);
   }

   if (where == NULL) {
      if (lib->n_units == 0) {
         lib->units_alloc = 16;
         lib->units = xmalloc_array(sizeof(lib_unit_t), lib->units_alloc);
      }
      else if (lib->n_units == lib->units_alloc) {
         lib->units_alloc *= 2;
         lib->units = xrealloc_array(lib->units,
                                     sizeof(lib_unit_t), lib->units_alloc);
      }

      where = &(lib->units[lib->n_units++]);
   }

   where->top      = unit;
   where->read_ctx = ctx;
   where->dirty    = dirty;
   where->mtime    = mtime;
   where->kind     = tree_kind(unit);

   lib_add_to_index(lib, name, tree_kind(unit));

   return where;
}

static lib_t lib_find_at(const char *name, const char *path, bool exact)
{
   char dir[PATH_MAX];
   char *p = dir + checked_sprintf(dir, sizeof(dir) - 4 - strlen(name),
                                   "%s" PATH_SEP, path);
   bool found = false;

   if (!exact) {
      // Append library name converting to lower case
      for (const char *n = name; *n != '\0'; n++)
         *p++ = tolower((int)*n);

      // Try suffixing standard revision extensions first
      for (vhdl_standard_t s = standard(); (s > STD_87) && !found; s--) {
         checked_sprintf(p, 4, ".%s", standard_suffix(s));
         found = (access(dir, F_OK) == 0);
      }
   }

   if (!found) {
      *p = '\0';
      if (access(dir, F_OK) < 0)
         return NULL;
   }

   char marker[PATH_MAX];
   checked_sprintf(marker, sizeof(marker), "%s" PATH_SEP "_NVC_LIB", dir);
   if (access(marker, F_OK) < 0)
      return NULL;

   return lib_init(name, dir, -1);
}

static text_buf_t *lib_file_path(lib_t lib, const char *name)
{
   text_buf_t *tb = tb_new();
   tb_printf(tb, "%s" PATH_SEP "%s", lib->path, name);
   return tb;
}

lib_t lib_loaded(ident_t name_i)
{
   if (name_i == work_i && work != NULL)
      return work;

   for (lib_list_t *it = loaded; it != NULL; it = it->next) {
      if (lib_name(it->item) == name_i && it->standard == standard())
         return it->item;
   }

   return NULL;
}

lib_t lib_new(const char *name, const char *path)
{
   ident_t name_i = upcase_name(name);

   lib_t lib = lib_loaded(name_i);
   if (lib != NULL)
      return lib;

   char *name_copy LOCAL = xstrdup(name);
   char *sep = strrchr(name_copy, '/');

   // Ignore trailing slashes
   while ((sep != NULL) && (*(sep + 1) == '\0')) {
      *sep = '\0';
      sep = strrchr(name_copy, '/');
   }

   if (sep != NULL) {
      // Work library contains explicit path
      *sep = '\0';
      name = sep + 1;
      lib = lib_find_at(name, name_copy, false);
   }
   else {
      // Look only in working directory
      lib = lib_find_at(name, ".", false);
   }

   if (lib != NULL)
      return lib;

   const char *last_slash = strrchr(name, '/');
   const char *last_dot = strrchr(name, '.');

   if ((last_dot != NULL) && (last_dot > last_slash)) {
      const char *ext = standard_suffix(standard());
      if (strcmp(last_dot + 1, ext) != 0)
         fatal("library directory suffix must be '%s' for this standard", ext);
   }

   for (const char *p = (last_slash ? last_slash + 1 : name);
        (*p != '\0') && (p != last_dot);
        p++) {
      if (!isalnum((int)*p) && (*p != '_'))
         fatal("invalid character '%c' in library name", *p);
   }

   char *lockf LOCAL = xasprintf("%s" PATH_SEP "%s", path, "_NVC_LIB");

   struct stat buf;
   if (stat(path, &buf) == 0) {
      if (S_ISDIR(buf.st_mode)) {
         struct stat sb;
         if (stat(lockf, &sb) != 0 && !opt_get_int("force-init"))
            fatal("directory %s already exists and is not an NVC library "
                  "(use --force-init to override this check)", path);
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

      const char *marker = PACKAGE_STRING "\n";
      if (write(fd, marker, strlen(marker)) < 0)
         fatal_errno("write: %s", path);
   }

   return lib_init(name, path, fd);
}

lib_t lib_at(const char *path)
{
   for (lib_list_t *it = loaded; it != NULL; it = it->next) {
      if (strncmp(path, it->item->path, strlen(it->item->path)) == 0
          && it->standard == standard())
         return it->item;
   }

   return NULL;
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
   if (search_paths == NULL) {
      push_path(DATADIR);

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
}

const char *lib_enum_search_paths(void **token)
{
   if (*token == NULL) {
      lib_default_search_paths();
      *token = search_paths;
   }

   if (*token == (void *)-1)
      return NULL;
   else {
      search_path_t *old = *token;
      if ((*token = old->next) == NULL)
         *token = (void *)-1;
      return old->path;
   }
}

void lib_add_search_path(const char *path)
{
   lib_default_search_paths();
   push_path(path);
}

void lib_add_map(const char *name, const char *path)
{
   lib_t lib = lib_find_at(name, path, true);
   if (lib == NULL)
      warnf("library %s not found at %s", name, path);
}

lib_t lib_find(ident_t name_i, bool required)
{
   lib_t lib = lib_loaded(name_i);
   if (lib != NULL)
      return lib;

   lib_default_search_paths();

   const char *name_str = istr(name_i);
   for (search_path_t *it = search_paths; it != NULL; it = it->next) {
      if ((lib = lib_find_at(name_str, it->path, false)))
         break;
   }

   if (lib == NULL) {
      LOCAL_TEXT_BUF tb = tb_new();
      tb_printf(tb, "library %s not found in:\n", name_str);
      for (search_path_t *it = search_paths; it != NULL; it = it->next)
         tb_printf(tb, "  %s\n", it->path);
      if (required)
         fatal("%s", tb_get(tb));
      else
         error_at(NULL, "%s", tb_get(tb));
   }

   return lib;
}

FILE *lib_fopen(lib_t lib, const char *name, const char *mode)
{
   assert(lib != NULL);
   LOCAL_TEXT_BUF path = lib_file_path(lib, name);
   return fopen(tb_get(path), mode);
}

fbuf_t *lib_fbuf_open(lib_t lib, const char *name, fbuf_mode_t mode)
{
   assert(lib != NULL);
   if (lib->path[0] == '\0')
      return NULL;
   else {
      LOCAL_TEXT_BUF path = lib_file_path(lib, name);
      return fbuf_open(tb_get(path), mode);
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

   if (lib->units != NULL)
      free(lib->units);

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
         checked_sprintf(buf, sizeof(buf), "%s" PATH_SEP "%s",
                         lib->path, e->d_name);
         if (unlink(buf) < 0)
            perror("unlink");
      }
   }

   closedir(d);

   if (rmdir(lib->path) < 0)
      perror("rmdir");
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

static lib_mtime_t lib_time_to_usecs(time_t t)
{
   return (lib_mtime_t)t * 1000 * 1000;
}

void lib_put(lib_t lib, tree_t unit)
{
   struct timeval tv;
   if (gettimeofday(&tv, NULL) != 0)
      fatal_errno("gettimeofday");

   lib_mtime_t usecs = ((lib_mtime_t)tv.tv_sec * 1000000) + tv.tv_usec;
   lib_put_aux(lib, unit, NULL, true, usecs);
}

static lib_mtime_t lib_stat_mtime(struct stat *st)
{
   lib_mtime_t mt = lib_time_to_usecs(st->st_mtime);
#if defined HAVE_STRUCT_STAT_ST_MTIMESPEC_TV_NSEC
   mt += st->st_mtimespec.tv_nsec / 1000;
#elif defined HAVE_STRUCT_STAT_ST_MTIM_TV_NSEC
   mt += st->st_mtim.tv_nsec / 1000;
#endif
   return mt;
}

static lib_unit_t *lib_get_aux(lib_t lib, ident_t ident)
{
   assert(lib != NULL);

   // Handle aliased library names and names without the library prefix
   ident_t lname = ident_until(ident, '.');
   if (lname != lib->name) {
      ident_t uname = ident_rfrom(ident, '.') ?: ident;
      ident = ident_prefix(lib->name, uname, '.');
   }

   // Search in the list of already loaded units
   for (unsigned n = 0; n < lib->n_units; n++) {
      if (tree_ident(lib->units[n].top) == ident)
         return &(lib->units[n]);
   }

   if (*(lib->path) == '\0')   // Temporary library
      return NULL;

   assert(lib->lock_fd != -1);   // Should not be called in unit tests
   file_read_lock(lib->lock_fd);

   // Otherwise search in the filesystem
   DIR *d = opendir(lib->path);
   if (d == NULL)
      fatal("%s: %s", lib->path, strerror(errno));

   lib_unit_t *unit = NULL;
   const char *search = istr(ident);
   struct dirent *e;
   while ((e = readdir(d))) {
      if (strcmp(e->d_name, search) == 0) {
         fbuf_t *f = lib_fbuf_open(lib, e->d_name, FBUF_IN);
         LOCAL_TEXT_BUF path = lib_file_path(lib, e->d_name);
         tree_rd_ctx_t ctx = tree_read_begin(f, tb_get(path));
         tree_t top = tree_read(ctx);
         fbuf_close(f);

         struct stat st;
         if (stat(tb_get(path), &st) < 0)
            fatal_errno("%s", e->d_name);

         lib_mtime_t mt = lib_stat_mtime(&st);

         unit = lib_put_aux(lib, top, ctx, false, mt);
         break;
      }
   }

   closedir(d);
   file_unlock(lib->lock_fd);

   if (unit == NULL && lib_find_in_index(lib, ident) != NULL)
      fatal("library %s corrupt: unit %s present in index but missing "
            "on disk", istr(lib->name), istr(ident));

   return unit;
}

static void lib_ensure_writable(lib_t lib)
{
   if (lib->readonly)
      fatal("cannot write to read-only library %s", istr(lib->name));
}

bool lib_load_vcode(lib_t lib, ident_t unit_name)
{
   if (lib->lock_fd != -1)
      file_read_lock(lib->lock_fd);

   char *name LOCAL = vcode_file_name(unit_name);
   fbuf_t *f = lib_fbuf_open(lib, name, FBUF_IN);
   if (f != NULL) {
      vcode_read(f);
      fbuf_close(f);
   }

   if (lib->lock_fd != -1)
      file_unlock(lib->lock_fd);
   return f != NULL;
}

void lib_save_vcode(lib_t lib, vcode_unit_t vu, ident_t unit_name)
{
   lib_ensure_writable(lib);

   if (lib->lock_fd != -1)
      file_write_lock(lib->lock_fd);

   char *name LOCAL = vcode_file_name(unit_name);
   fbuf_t *fbuf = lib_fbuf_open(lib, name, FBUF_OUT);
   vcode_write(vu, fbuf);
   fbuf_close(fbuf);

   if (lib->lock_fd != -1)
      file_unlock(lib->lock_fd);
}

lib_mtime_t lib_mtime(lib_t lib, ident_t ident)
{
   lib_unit_t *lu = lib_get_aux(lib, ident);
   assert(lu != NULL);
   return lu->mtime;
}

bool lib_stat(lib_t lib, const char *name, lib_mtime_t *mt)
{
   struct stat buf;
   LOCAL_TEXT_BUF path = lib_file_path(lib, name);
   if (stat(tb_get(path), &buf) == 0) {
      if (mt != NULL)
         *mt = lib_stat_mtime(&buf);
      return true;
   }
   else
      return false;
}

tree_t lib_get(lib_t lib, ident_t ident)
{
   lib_unit_t *lu = lib_get_aux(lib, ident);
   if (lu != NULL) {
      if (lu->read_ctx != NULL) {
         tree_read_end(lu->read_ctx);
         lu->read_ctx = NULL;
      }
      return lu->top;
   }
   else
      return NULL;
}

tree_t lib_get_check_stale(lib_t lib, ident_t ident)
{
   lib_unit_t *lu = lib_get_aux(lib, ident);
   if (lu != NULL) {
      if (lu->read_ctx != NULL) {
         tree_read_end(lu->read_ctx);
         lu->read_ctx = NULL;
      }

      if (!opt_get_int("ignore-time")) {
         const loc_t *loc = tree_loc(lu->top);

         struct stat st;
         if (stat(loc_file_str(loc), &st) == 0 && lu->mtime < lib_stat_mtime(&st))
            fatal("design unit %s is older than its source file %s and must "
                  "be reanalysed\n(You can use the --ignore-time option to "
                  "skip this check)", istr(ident), loc_file_str(loc));
      }

      return lu->top;
   }
   else
      return NULL;
}

tree_t lib_get_qualified(ident_t qual)
{
   ident_t lib_name = ident_until(qual, '.');
   if (lib_name == NULL)
      return NULL;

   lib_t lib = lib_find(lib_name, false);
   if (lib == NULL)
      return NULL;

   return lib_get_check_stale(lib, qual);
}

ident_t lib_name(lib_t lib)
{
   assert(lib != NULL);
   return lib->name;
}

void lib_save(lib_t lib)
{
   assert(lib != NULL);

   assert(lib->lock_fd != -1);   // Should not be called in unit tests
   lib_ensure_writable(lib);
   file_write_lock(lib->lock_fd);

   for (unsigned n = 0; n < lib->n_units; n++) {
      if (lib->units[n].dirty) {
         const char *name = istr(tree_ident(lib->units[n].top));
         fbuf_t *f = lib_fbuf_open(lib, name, FBUF_OUT);
         if (f == NULL)
            fatal("failed to create %s in library %s", name, istr(lib->name));
         tree_wr_ctx_t ctx = tree_write_begin(f);
         tree_write(lib->units[n].top, ctx);
         tree_write_end(ctx);
         fbuf_close(f);

         lib->units[n].dirty = false;
      }
   }

   LOCAL_TEXT_BUF index_path = lib_file_path(lib, "_index");
   struct stat st;
   if (stat(tb_get(index_path), &st) == 0
       && (lib_stat_mtime(&st) != lib->index_mtime
           || st.st_size != lib->index_size)) {
      // Library was updated concurrently: re-read the index while we
      // have the lock
      lib_read_index(lib);
   }

   int index_sz = lib_index_size(lib);

   fbuf_t *f = lib_fbuf_open(lib, "_index", FBUF_OUT);
   if (f == NULL)
      fatal_errno("failed to create library %s index", istr(lib->name));

   ident_wr_ctx_t ictx = ident_write_begin(f);

   write_u32(index_sz, f);
   for (lib_index_t *it = lib->index; it != NULL; it = it->next) {
      ident_write(it->name, ictx);
      write_u16(it->kind, f);
   }

   ident_write_end(ictx);
   fbuf_close(f);

   if (stat(tb_get(index_path), &st) != 0)
      fatal_errno("stat: %s", tb_get(index_path));
   lib->index_mtime = lib_stat_mtime(&st);
   lib->index_size  = st.st_size;

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

   if (name)
      checked_sprintf(buf, buflen, "%s" PATH_SEP "%s", lib->path, name);
   else
      strncpy(buf, lib->path, buflen);
}

void lib_mkdir(lib_t lib, const char *name)
{
   LOCAL_TEXT_BUF path = lib_file_path(lib, name);
   make_dir(tb_get(path));
}

void lib_delete(lib_t lib, const char *name)
{
   assert(lib != NULL);
   LOCAL_TEXT_BUF path = lib_file_path(lib, name);
   if (remove(tb_get(path)) != 0 && errno != ENOENT)
      fatal_errno("remove: %s", name);
}
