AC_INIT([nhdl], [0.1],
        [Nick Gasson <nick@nick.me.uk>],
        [nhdl])
AM_INIT_AUTOMAKE([-Wall color-tests])

AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_MKDIR_P
     
AC_FUNC_MALLOC
AC_TYPE_SIZE_T
AC_CHECK_FUNCS([strdup mkdir realpath rmdir strerror])
AC_CHECK_HEADERS([limits.h stdlib.h stddef.h])

AC_CONFIG_HEADERS([config.h])
AC_OUTPUT([Makefile src/Makefile test/Makefile])
