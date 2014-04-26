#include "ident.h"

#include <stdlib.h>
#include <string.h>
#include <assert.h>

int main(int argc, char **argv)
{
   for (int i = 0; i < 10000000; i++) {
      char buf[16];
      size_t len = (random() % (sizeof(buf) - 3)) + 2;

      for (size_t j = 0; j < len; j++)
         buf[j] = '0' + (random() % 80);
      buf[len - 1] = '\0';

      ident_t i1 = ident_new(buf);
      assert(i1 != NULL);
      assert(strcmp(istr(i1), buf) == 0);
   }

   return 0;
}
