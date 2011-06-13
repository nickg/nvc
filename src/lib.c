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

   struct lib *l = xmalloc(sizeof(struct lib));
   realpath(name, l->path);
   
   return l;
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

