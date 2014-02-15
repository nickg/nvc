//
//  Copyright (C) 2011-2014  Nick Gasson
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

#define _GNU_SOURCE

#include "util.h"
#include "lib.h"
#include "tree.h"
#include "common.h"

#include <assert.h>
#include <limits.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>
#include <dirent.h>
#include <errno.h>
#include <time.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/time.h>

typedef struct search_path search_path_t;

struct lib_unit {
   tree_t        top;
   tree_kind_t   kind;
   tree_rd_ctx_t read_ctx;
   bool          dirty;
   lib_mtime_t   mtime;
};

struct lib_index {
   ident_t           name;
   tree_kind_t       kind;
   struct lib_index *next;
};

struct lib {
   char              path[PATH_MAX];
   ident_t           name;
   unsigned          n_units;
   unsigned          units_alloc;
   struct lib_unit  *units;
   struct lib_index *index;
};

struct lib_list {
   lib_t           item;
   struct lib_list *next;
};

struct search_path {
   search_path_t *next;
   const char    *path;
};

static lib_t            work = NULL;
static struct lib_list *loaded = NULL;
static search_path_t   *search_paths = NULL;

static const char *standard_suffix(vhdl_standard_t std)
{
   static const char *ext[] = {
      "87", "93", "00", "02", "08"
   };

   assert(std < ARRAY_LEN(ext));
   return ext[std];
}

static ident_t upcase_name(const char * name)
{
   char *name_copy = strdup(name);

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

   ident_t i = ident_new(name_up);
   free(name_copy);
   return i;
}

static lib_t lib_init(const char *name, const char *rpath)
{
   struct lib *l = xmalloc(sizeof(struct lib));
   l->n_units = 0;
   l->units   = NULL;
   l->name    = upcase_name(name);
   l->index   = NULL;

   if (realpath(rpath, l->path) == NULL)
      strncpy(l->path, rpath, PATH_MAX);

   struct lib_list *el = xmalloc(sizeof(struct lib_list));
   el->item = l;
   el->next = loaded;
   loaded = el;

   fbuf_t *f = lib_fbuf_open(l, "_index", FBUF_IN);
   if (f != NULL) {
      ident_rd_ctx_t ictx = ident_read_begin(f);

      const int entries = read_u32(f);
      for (int i = 0; i < entries; i++) {
         ident_t name = ident_read(ictx);
         tree_kind_t kind = read_u16(f);
         assert(kind < T_LAST_TREE_KIND);

         struct lib_index *in = xmalloc(sizeof(struct lib_index));
         in->name = name;
         in->kind = kind;
         in->next = l->index;

         l->index = in;
      }

      ident_read_end(ictx);
      fbuf_close(f);
   }

   return l;
}

static struct lib_unit *lib_put_aux(lib_t lib, tree_t unit,
                                    tree_rd_ctx_t ctx, bool dirty,
                                    lib_mtime_t mtime)
{
   assert(lib != NULL);
   assert(unit != NULL);

   if (lib->n_units == 0) {
      lib->units_alloc = 16;
      lib->units = xmalloc(sizeof(struct lib_unit) * lib->units_alloc);
   }
   else if (lib->n_units == lib->units_alloc) {
      lib->units_alloc *= 2;
      lib->units = xrealloc(lib->units,
                            sizeof(struct lib_unit) * lib->units_alloc);
   }

   unsigned n = lib->n_units++;
   lib->units[n].top      = unit;
   lib->units[n].read_ctx = ctx;
   lib->units[n].dirty    = dirty;
   lib->units[n].mtime    = mtime;
   lib->units[n].kind     = tree_kind(unit);

   ident_t name = tree_ident(unit);
   struct lib_index *it;
   for (it = lib->index;
        (it != NULL) && (it->name != name);
        it = it->next)
      ;

   if (it == NULL) {
      struct lib_index *new = xmalloc(sizeof(struct lib_index));
      new->name = name;
      new->kind = tree_kind(unit);
      new->next = lib->index;

      lib->index = new;
   }
   else
      it->kind = tree_kind(unit);

   return &(lib->units[n]);
}

static lib_t lib_find_at(const char *name, const char *path)
{
   char dir[PATH_MAX];
   const int nchars = snprintf(dir, sizeof(dir) - 4, "%s/%s", path, name);

   // Convert to lower case
   for (char *p = dir; *p != '\0'; p++)
      *p = tolower((int)*p);

   // Try suffixing standard revision extensions first
   bool found = false;
   for (vhdl_standard_t s = standard(); (s > STD_87) && !found; s--) {
      snprintf(dir + nchars, 4, ".%s", standard_suffix(s));
      found = (access(dir, F_OK) == 0);
   }

   if (!found) {
      dir[nchars] = '\0';
      if (access(dir, F_OK) < 0)
         return NULL;
   }

   char marker[PATH_MAX];
   snprintf(marker, sizeof(marker), "%s/_NVC_LIB", dir);
   if (access(marker, F_OK) < 0)
      return NULL;

   return lib_init(name, dir);
}

static const char *lib_file_path(lib_t lib, const char *name)
{
   static char buf[PATH_MAX];
   snprintf(buf, sizeof(buf), "%s/%s", lib->path, name);
   return buf;
}

lib_t lib_new(const char *name)
{
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
      if (!isalnum((int)*p))
         fatal("invalid character '%c' in library name", *p);
   }

   if (access(name, F_OK) == 0)
      fatal("file %s already exists", name);

   if (mkdir(name, 0777) != 0)
      fatal_errno("mkdir: %s", name);

   lib_t l = lib_init(name, name);

   FILE *tag = lib_fopen(l, "_NVC_LIB", "w");
   fprintf(tag, "%s\n", PACKAGE_STRING);
   fclose(tag);

   return l;
}

lib_t lib_tmp(void)
{
   // For unit tests, avoids creating files
   return lib_init("work", "");
}

static void push_path(const char *path)
{
   search_path_t *s = xmalloc(sizeof(search_path_t));
   s->next = search_paths;
   s->path = path;

   search_paths = s;
}

static void lib_default_search_paths(void)
{
   if (search_paths == NULL) {
      push_path(DATADIR);

      const char *home_env = getenv("HOME");
      if (home_env) {
         char *path;
         if (asprintf(&path, "%s/.%s/lib", home_env, PACKAGE) < 0)
            fatal_errno("asprintf");
         push_path(path);
      }

      char *env_copy = NULL;
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
   push_path(strdup(path));
}

lib_t lib_find(const char *name, bool verbose, bool search)
{
   // Search in already loaded libraries
   ident_t name_i = upcase_name(name);
   for (struct lib_list *it = loaded; it != NULL; it = it->next) {
      if (lib_name(it->item) == name_i)
         return it->item;
   }

   lib_t lib = NULL;

   char *name_copy = strdup(name);
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
      lib = lib_find_at(name, name_copy);
   }
   else if (search) {
      lib_default_search_paths();

      for (search_path_t *it = search_paths; it != NULL; it = it->next) {
         if ((lib = lib_find_at(name, it->path)))
            break;
      }

      if ((lib == NULL) && verbose) {
         char buf[2048];
         static_printf_begin(buf, sizeof(buf));
         static_printf(buf, "library %s not found in:\n", name);
         for (search_path_t *it = search_paths; it != NULL; it = it->next)
            static_printf(buf, "  %s\n", it->path);
         errorf("%s", buf);
      }
   }
   else {
      // Look only in working directory
      lib = lib_find_at(name, ".");
   }

   free(name_copy);
   return lib;
}

FILE *lib_fopen(lib_t lib, const char *name, const char *mode)
{
   assert(lib != NULL);
   return fopen(lib_file_path(lib, name), mode);
}

fbuf_t *lib_fbuf_open(lib_t lib, const char *name, fbuf_mode_t mode)
{
   assert(lib != NULL);
   return fbuf_open(lib_file_path(lib, name), mode);
}

void lib_free(lib_t lib)
{
   assert(lib != NULL);

   for (struct lib_list *it = loaded, *prev = NULL;
        it != NULL; loaded = it, it = it->next) {

      if (it->item == lib) {
         if (prev)
            prev->next = it->next;
         else
            loaded = it->next;
         free(it);
         break;
      }
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

   DIR *d = opendir(lib->path);
   if (d == NULL) {
      perror("opendir");
      return;
   }

   char buf[PATH_MAX];
   struct dirent *e;
   while ((e = readdir(d))) {
      if (e->d_name[0] != '.') {
         snprintf(buf, sizeof(buf), "%s/%s", lib->path, e->d_name);
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

static struct lib_unit *lib_get_aux(lib_t lib, ident_t ident)
{
   assert(lib != NULL);

   // Search in the list of already loaded libraries
   for (unsigned n = 0; n < lib->n_units; n++) {
      if (tree_ident(lib->units[n].top) == ident)
         return &(lib->units[n]);
   }

   if (*(lib->path) == '\0')   // Temporary library
      return NULL;

   // Otherwise search in the filesystem
   DIR *d = opendir(lib->path);
   if (d == NULL)
      fatal("%s: %s", lib->path, strerror(errno));

   struct lib_unit *unit = NULL;
   const char *search = istr(ident);
   struct dirent *e;
   while ((e = readdir(d))) {
      if (strcmp(e->d_name, search) == 0) {
         fbuf_t *f = lib_fbuf_open(lib, e->d_name, FBUF_IN);
         tree_rd_ctx_t ctx = tree_read_begin(f, lib_file_path(lib, e->d_name));
         tree_t top = tree_read(ctx);

         struct stat st;
         if (stat(lib_file_path(lib, e->d_name), &st) < 0)
            fatal_errno("%s", e->d_name);

         lib_mtime_t mt = lib_stat_mtime(&st);

         unit = lib_put_aux(lib, top, ctx, false, mt);
         break;
      }
   }

   closedir(d);
   return unit;
}

lib_mtime_t lib_mtime(lib_t lib, ident_t ident)
{
   struct lib_unit *lu = lib_get_aux(lib, ident);
   assert(lu != NULL);
   return lu->mtime;
}

bool lib_stat(lib_t lib, const char *name, lib_mtime_t *mt)
{
   struct stat buf;
   if (stat(lib_file_path(lib, name), &buf) == 0) {
      if (mt != NULL)
         *mt = lib_stat_mtime(&buf);
      return true;
   }
   else
      return false;
}

tree_t lib_get_ctx(lib_t lib, ident_t ident, tree_rd_ctx_t *ctx)
{
   struct lib_unit *lu = lib_get_aux(lib, ident);
   if (lu != NULL) {
      *ctx = lu->read_ctx;
      return lu->top;
   }
   else
      return NULL;
}

tree_t lib_get(lib_t lib, ident_t ident)
{
   struct lib_unit *lu = lib_get_aux(lib, ident);
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
   struct lib_unit *lu = lib_get_aux(lib, ident);
   if (lu != NULL) {
      if (lu->read_ctx != NULL) {
         tree_read_end(lu->read_ctx);
         lu->read_ctx = NULL;
      }

      const loc_t *loc = tree_loc(lu->top);

      struct stat st;
      if ((stat(loc->file, &st) == 0) && (lu->mtime < lib_stat_mtime(&st)))
         fatal("design unit %s is older than its source file %s and must "
               "be reanalysed", istr(ident), loc->file);

      return lu->top;
   }
   else
      return NULL;
}

ident_t lib_name(lib_t lib)
{
   assert(lib != NULL);
   return lib->name;
}

void lib_save(lib_t lib)
{
   assert(lib != NULL);

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

   struct lib_index *it;
   int index_sz = 0;
   for (it = lib->index; it != NULL; it = it->next, ++index_sz)
      ;

   fbuf_t *f = lib_fbuf_open(lib, "_index", FBUF_OUT);
   if (f == NULL)
      fatal("failed to create library %s index", istr(lib->name));

   ident_wr_ctx_t ictx = ident_write_begin(f);

   write_u32(index_sz, f);
   for (it = lib->index; it != NULL; it = it->next) {
      ident_write(it->name, ictx);
      write_u16(it->kind, f);
   }

   ident_write_end(ictx);
   fbuf_close(f);
}

void lib_walk_index(lib_t lib, lib_index_fn_t fn, void *context)
{
   assert(lib != NULL);

   struct lib_index *it;
   for (it = lib->index; it != NULL; it = it->next)
      (*fn)(it->name, it->kind, context);
}

unsigned lib_index_size(lib_t lib)
{
   assert(lib != NULL);

   unsigned n = 0;
   for (struct lib_index *it = lib->index; it != NULL; it = it->next)
      n++;

   return n;
}

void lib_realpath(lib_t lib, const char *name, char *buf, size_t buflen)
{
   assert(lib != NULL);

   if (name)
      snprintf(buf, buflen, "%s/%s", lib->path, name);
   else
      strncpy(buf, lib->path, buflen);
}

void lib_mkdir(lib_t lib, const char *name)
{
   if ((mkdir(lib_file_path(lib, name), 0777) != 0)
       && (errno != EEXIST))
      fatal_errno("mkdir: %s", name);
}
