#include "util.h"
#include "lib.h"

#include <assert.h>
#include <limits.h>
#include <stdlib.h>
#include <unistd.h>
#include <dirent.h>
#include <sys/stat.h>
#include <sys/types.h>

struct lib {
   char path[PATH_MAX];
};

static lib_t lib_init(const char *rpath)
{
   struct lib *l = xmalloc(sizeof(struct lib));
   realpath(rpath, l->path);

   return l;
}

lib_t lib_new(const char *name)
{
   if (access(name, F_OK) == 0) {
      errorf("file %s already exists", name);
      return NULL;
   }

   if (mkdir(name, 0777) != 0) {
      perrorf("mkdir");
      return NULL;
   }

   lib_t l = lib_init(name);

   FILE *tag = lib_fopen(l, "_NHDL_LIB", "w");
   fprintf(tag, "%s\n", PACKAGE_STRING);
   fclose(tag);

   return l;
}

lib_t lib_find(const char *name)
{
   if (access(name, F_OK) < 0)
      return NULL;

   char buf[PATH_MAX];
   snprintf(buf, sizeof(buf), "%s/_NHDL_LIB", name);
   if (access(buf, F_OK) < 0)
      return NULL;

   return lib_init(name);
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
   free(lib);
}

void lib_destroy(lib_t lib)
{
   // This is convenience function for testing: remove all
   // files associated with a library
   
   assert(lib != NULL);

   DIR *d = opendir(lib->path);
   if (d == NULL) {
      perrorf("opendir");
      return;
   }

   char buf[PATH_MAX];
   struct dirent *e;
   while ((e = readdir(d))) {
      if (e->d_name[0] != '.') {
         snprintf(buf, sizeof(buf), "%s/%s", lib->path, e->d_name);
         if (unlink(buf) < 0)
            perrorf("unlink");
      }
   }

   closedir(d);

   if (rmdir(lib->path) < 0)
      perrorf("rmdir");
}

