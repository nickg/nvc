#include "lib.h"

#include <assert.h>
#include <limits.h>
#include <stdlib.h>

struct lib {
   char path[PATH_MAX];
};

lib_t lib_new(const char *name)
{
   return NULL;
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
}

