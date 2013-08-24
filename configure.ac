AC_INIT([nvc], [0.1],
        [Nick Gasson <nick@nick.me.uk>],
        [nvc])
AM_INIT_AUTOMAKE([1.12 color-tests serial-tests])
AM_SILENT_RULES([yes])

AC_CONFIG_MACRO_DIR([m4])

AC_CANONICAL_HOST

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
AC_TYPE_SSIZE_T
AC_TYPE_INT8_T
AC_TYPE_INT16_T
AC_TYPE_INT32_T
AC_TYPE_INT64_T
AC_TYPE_UINT8_T
AC_TYPE_UINT16_T
AC_TYPE_UINT32_T
AC_TYPE_UINT64_T
AC_TYPE_PID_T

AC_FUNC_MMAP   
AC_FUNC_ALLOCA 
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_FUNC_FORK
AC_CHECK_FUNCS([strdup mkdir realpath rmdir strerror strcasecmp \
                memset strchr strrchr strstr setenv])

AC_HEADER_STDBOOL
AC_CHECK_HEADERS([limits.h stddef.h fcntl.h libintl.h malloc.h \
                  sys/socket.h])

AC_CHECK_MEMBERS([struct stat.st_mtimespec.tv_nsec])
AC_CHECK_MEMBERS([struct stat.st_mtim.tv_nsec])

AC_CHECK_HEADERS([tcl.h tcl/tcl.h], [have_tcl=yes; break], [])
AC_SEARCH_LIBS([Tcl_CreateInterp], [tcl], [],
  [AC_ERROR(TCL library not found)])

if test ! "$have_tcl" = "yes"; then
  AC_ERROR(TCL header file not found)
fi

VL_LIB_READLINE
if test "$vl_cv_lib_readline" = "no"; then
  AC_WARN(terminal line editing will not be supported)
fi

AC_CHECK_LIB([z], [deflate], [], [AC_ERROR([zlib not found])])
AC_CHECK_LIB([bz2], [BZ2_bzclose], [], [AC_ERROR([bzip2 not found])])

AX_LLVM_C([engine bitreader bitwriter])
AM_CONDITIONAL([FORCE_CXX_LINK], [test "$ax_cv_llvm_shared" != yes])

PKG_CHECK_MODULES([CHECK], [check >= 0.9.4], [], [])

AX_DEFINE_DIR([DATADIR], [datadir/nvc], [Installation data directory])

# On cygwin local headers can sometimes be included instead
# of system ones which is worked around using -I-. A better
# solution would be to have automake use -iquote.
case $host_os in
  *cygwin* ) CFLAGS="$CFLAGS -I-" ;;
esac

AC_DEFINE_UNQUOTED([SYSTEM_CC], ["$(which $CC)"], [System compiler])

# Turn on silent make when using the GNU version
AX_CHECK_GNU_MAKE
if ! test x$_cv_gnu_make_command = x ; then
  MAKEFLAGS='$(if $(V),,-s)'
fi
AC_SUBST(MAKEFLAGS)

# Optional code coverage statistics
AC_ARG_ENABLE([coverage],
  [AS_HELP_STRING([--enable-coverage],
    [Enable gcov for code coverage statistics])],
    [enable_coverage=yes],
    [])
if test x$enable_coverage = xyes ; then
  COV_CFLAGS="--coverage --no-inline"
  COV_LDFLAGS="--coverage"
fi
AC_SUBST(COV_CFLAGS)
AC_SUBST(COV_LDFLAGS)
AM_CONDITIONAL([ENABLE_COVERAGE], [test "$enable_coverage" = yes])

AC_CONFIG_HEADERS([config.h])
AC_OUTPUT([Makefile src/Makefile src/rt/Makefile test/Makefile \
           lib/Makefile lib/std/Makefile lib/ieee/Makefile \
           lib/synopsys/Makefile lxt/Makefile])

