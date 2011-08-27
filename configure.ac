AC_INIT([nhdl], [0.1],
        [Nick Gasson <nick@nick.me.uk>],
        [nhdl])
AM_INIT_AUTOMAKE([color-tests])

AC_PROG_CC
AC_PROG_CC_C99
AM_PROG_CC_C_O
AC_PROG_CXX
AC_PROG_INSTALL
AC_PROG_MKDIR_P
AC_PROG_YACC
AM_PROG_LEX
AC_PROG_RANLIB

AC_C_INLINE

AC_TYPE_SIZE_T
AC_TYPE_INT16_T
AC_TYPE_INT32_T
AC_TYPE_INT8_T
AC_TYPE_UINT16_T
AC_TYPE_UINT32_T
AC_TYPE_UINT8_T
AC_TYPE_PID_T

AC_FUNC_MMAP   
AC_FUNC_ALLOCA 
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_FUNC_FORK
AC_CHECK_FUNCS([strdup mkdir realpath rmdir strerror strcasecmp \
                memset strchr strrchr strstr])

AC_HEADER_STDBOOL
AC_CHECK_HEADERS([limits.h stddef.h fcntl.h libintl.h malloc.h])

AX_LLVM_C([core bitwriter])

AX_DEFINE_DIR([DATADIR], [datadir/nhdl], [Installation data directory])

AC_CONFIG_HEADERS([config.h])
AC_OUTPUT([Makefile src/Makefile test/Makefile lib/Makefile \
           lib/std/Makefile])
