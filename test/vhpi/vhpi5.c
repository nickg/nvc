#include "vhpi_user.h"

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>

#define fail_if(x)                                                      \
   if (x) vhpi_assert(vhpiFailure, "assertion '%s' failed at %s:%d",    \
                      #x, __FILE__, __LINE__)
#define fail_unless(x) fail_if(!(x))

static void check_error(void)
{
   vhpiErrorInfoT info;
   if (vhpi_check_error(&info))
      vhpi_assert(vhpiFailure, "unexpected error '%s'", info.message);
}

static void startup()
{
   vhpi_printf("hello, world!");
   vhpi_printf("tool is %s", vhpi_get_str(vhpiNameP, NULL));

   vhpiHandleT root = vhpi_handle(vhpiRootInst, NULL);
   check_error();
   fail_if(root == NULL);
   vhpi_printf("root handle %p", root);

   const vhpiCharT *root_name = vhpi_get_str(vhpiNameP, root);
   vhpi_printf("root name is %s", root_name);

   vhpiHandleT s1 = vhpi_handle_by_name("s1", root);
   check_error();
   fail_if(s1 == NULL);
   vhpi_printf("s1 handle %p", s1);

   vhpiHandleT s1_type = vhpi_handle(vhpiType, s1);
   check_error();
   fail_if(s1_type == NULL);

   // TODO: check fields, etc.

   vhpi_release_handle(root);
}

void (*vhpi_startup_routines[])() = {
   startup,
   NULL
};
