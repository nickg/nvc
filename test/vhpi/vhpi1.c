#include "vhpi_user.h"

#include <stdio.h>

static void startup()
{
   vhpi_printf("hello, world!");
}

void (*vhpi_startup_routines[])() = {
   startup,
   NULL
};
