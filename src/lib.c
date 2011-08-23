#include "util.h"
#include "lib.h"
#include "tree.h"

#include <assert.h>
#include <limits.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>
#include <dirent.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/types.h>

#define MAX_UNITS 16

struct lib {
   char     path[PATH_MAX];
   ident_t  name;
   unsigned n_units;
   tree_t   *units;
};

struct lib_list {
   lib_t           item;
   struct lib_list *next;
};

static lib_t           work = NULL;
static struct lib_list *loaded = NULL;

static ident_t upcase_name(const char *name)
{
   char *name_up = strdup(name);
   for (char *p = name_up; *p != '\0'; p++)
      *p = toupper(*p);
   
   ident_t i = ident_new(name_up);
   free(name_up);
   return i;
}

static lib_t lib_init(const char *name, const char *rpath, bool scan)
{
   struct lib *l = xmalloc(sizeof(struct lib));
   l->n_units = 0;
   l->units   = NULL;
   l->name    = upcase_name(name);
   
   realpath(rpath, l->path);

   // TODO: we probably want to lazy-load units
   if (scan) {
      DIR *d = opendir(l->path);
      if (d == NULL)
         fatal("%s: %s", l->path, strerror(errno));
      
      struct dirent *e;
      while ((e = readdir(d))) {
         if (e->d_name[0] != '.' && e->d_name[0] != '_') {
            FILE *f = lib_fopen(l, e->d_name, "r");
            printf("load from %s/%s\n", l->path, e->d_name);
            tree_rd_ctx_t ctx = tree_read_begin(f);
            lib_put(l, tree_read(ctx));
            tree_read_end(ctx);
            fclose(f);
         }
      }
      
      closedir(d);
   }

   struct lib_list *el = xmalloc(sizeof(struct lib_list));
   el->item = l;
   el->next = loaded;
   loaded = el;    

   return l;
}

static lib_t lib_find_at(const char *name, const char *path)
{
   char dir[PATH_MAX];
   snprintf(dir, sizeof(dir), "%s/%s", path, name);

   if (access(dir, F_OK) < 0)
      return NULL;

   char marker[PATH_MAX];
   snprintf(marker, sizeof(marker), "%s/_NHDL_LIB", dir);
   if (access(marker, F_OK) < 0)
      return NULL;

   return lib_init(name, dir, true);
}

lib_t lib_new(const char *name)
{
   if (access(name, F_OK) == 0) {
      errorf("file %s already exists", name);
      return NULL;
   }

   if (mkdir(name, 0777) != 0) {
      perror("mkdir");
      return NULL;
   }

   lib_t l = lib_init(name, name, false);

   FILE *tag = lib_fopen(l, "_NHDL_LIB", "w");
   fprintf(tag, "%s\n", PACKAGE_STRING);
   fclose(tag);

   return l;
}

lib_t lib_tmp(void)
{
   // For unit tests, avoids creating files
   return lib_init("work", "", false);
}

lib_t lib_find(const char *name, bool verbose)
{
   // Search in already loaded libraries
   ident_t name_i = upcase_name(name);
   for (struct lib_list *it = loaded; it != NULL; it = it->next) {
      if (lib_name(it->item) == name_i)
         return it->item;
   }
   
   const char *paths[] = {
      ".",
      "../lib/std",  // For unit tests (XXX: add NHDL_LIBPATH)
      DATADIR,
      NULL
   };

   lib_t lib;
   for (const char **p = paths; *p != NULL; p++) {
      if ((lib = lib_find_at(name, *p)))
         return lib;
   }

   if (verbose) {
      fprintf(stderr, "library %s not found in:\n", name);
      for (const char **p = paths; *p != NULL; p++) {
         fprintf(stderr, "  %s\n", *p);
      }
   }

   return NULL;
}

FILE *lib_fopen(lib_t lib, const char *name, const char *mode)
{
   assert(lib != NULL);
   
   char buf[PATH_MAX];
   snprintf(buf, sizeof(buf), "%s/%s", lib->path, name);

   return fopen(buf, mode);
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

void lib_put(lib_t lib, tree_t unit)
{
   assert(lib != NULL);
   assert(unit != NULL);
   assert(lib->n_units < MAX_UNITS);

   if (lib->n_units == 0)
      lib->units = xmalloc(sizeof(tree_t) * MAX_UNITS);

   lib->units[lib->n_units++] = unit;
}

tree_t lib_get(lib_t lib, ident_t ident)
{
   assert(lib != NULL);

   for (unsigned n = 0; n < lib->n_units; n++) {
      if (tree_ident(lib->units[n]) == ident)
         return lib->units[n];
   }

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
      FILE *f = lib_fopen(lib, istr(tree_ident(lib->units[n])), "w");
      tree_wr_ctx_t ctx = tree_write_begin(f);
      tree_write(lib->units[n], ctx);
      tree_write_end(ctx);
      fclose(f);
   }
}
