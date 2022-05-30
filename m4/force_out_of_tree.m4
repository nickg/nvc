# -*- mode: sh -*-
#
# Disallow in-tree builds as we keep library sources under lib/std,
# etc. which are also build outputs
#

AC_DEFUN([FORCE_OUT_OF_TREE],
  [if test "$srcdir" = "."; then
     AC_MSG_ERROR(
       [in-tree builds are not supported

        Use a separate build directory, for example:

            mkdir build && cd build
            ../configure$ac_configure_args_raw
       ])
   fi])
