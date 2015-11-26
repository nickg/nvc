AC_INIT([nvc], [0.1],
        [Nick Gasson <nick@nick.me.uk>],
        [nvc])

dnl Automake 1.11 does not support the serial-tests option
dnl Remove this when 1.11 no longer in common use
m4_define([serial_tests], [
  m4_esyscmd([
    automake --version |
      head -1 |
      awk '{split ($NF,a,"."); if (a[1] == 1 && a[2] >= 12) { print "serial-tests" }}'
  ])
])

m4_include([m4/autoconf-2.63_compat.m4])

AM_INIT_AUTOMAKE([1.11 -Wall -Wno-extra-portability color-tests
                  foreign subdir-objects] serial_tests)
AM_SILENT_RULES([yes])

AC_CONFIG_MACRO_DIR([m4])

AC_CANONICAL_HOST

AC_PROG_CC
AC_PROG_CC_C99
AM_PROG_CC_C_O
AC_PROG_CXX
AC_PROG_INSTALL
AC_PROG_MKDIR_P
AM_PROG_LEX
AC_PROG_RANLIB
AC_EXEEXT

AC_C_INLINE
AC_C_RESTRICT

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
AC_FUNC_FSEEKO
AC_FUNC_STRTOD
AC_CHECK_FUNCS([strdup mkdir realpath rmdir strerror strcasecmp \
                memset strchr strrchr strstr setenv])

AC_HEADER_STDBOOL
AC_CHECK_HEADERS([limits.h stddef.h fcntl.h libintl.h malloc.h \
                  sys/socket.h execinfo.h sys/ptrace.h sys/sysctl.h \
                  sys/prctl.h])

AC_CHECK_MEMBERS([struct stat.st_mtimespec.tv_nsec])
AC_CHECK_MEMBERS([struct stat.st_mtim.tv_nsec])

AX_CHECK_LINK_FLAG([-rdynamic],
  [RDYNAMIC_FLAG="-rdynamic"],
  [RDYNAMIC_FLAG=""])
AC_SUBST(RDYNAMIC_FLAG)

AX_CHECK_COMPILE_FLAG([-fPIC],
  [PIC_FLAG="-fPIC"],
  [PIC_FLAG=""],
  [-Werror])
AC_SUBST(PIC_FLAG)

AX_PROG_FLEX([], [AC_MSG_ERROR(GNU Flex not found)])

case $host_os in
  openbsd*)
    # Need -pthread for TCL on OpenBSD
    AX_PTHREAD
    LIBS="$PTHREAD_LIBS $LIBS"
    CFLAGS="$CFLAGS $PTHREAD_CFLAGS"
    LDFLAGS="$LDFLAGS $PTHREAD_LDFLAGS"

    # Need to link libexecinfo explicitly
    AC_SEARCH_LIBS([backtrace_symbols], [execinfo], [],
      [AC_MSG_ERROR(libexecinfo not found)], [])
    ;;

  *cygwin*)
    # LLVM on Cygwin needs libffi and curses
    AX_WITH_CURSES
    if test "x$ax_cv_curses" != xyes; then
      AC_MSG_ERROR([LLVM on Cygwin requires curses])
    fi

    AC_SEARCH_LIBS([ffi_call], [ffi], [],
      [AC_MSG_ERROR(LLVM on Cygwin requires libffi)], [])
    ;;
esac

AC_SEARCH_LIBS([dlopen], [dl dld], [], [
  AC_MSG_ERROR([unable to find the dlopen() function])
])

AC_CHECK_HEADERS([tcl.h tcl/tcl.h], [have_tcl=yes; break], [])
AC_SEARCH_LIBS([Tcl_CreateInterp], [tcl tcl86], [],
  [AC_MSG_ERROR(TCL library not found)], [])

if test ! x$have_tcl = xyes ; then
  AC_MSG_ERROR(TCL header file not found)
fi

AX_LIB_READLINE
if test "x$ax_cv_lib_readline" = xno ; then
  AC_MSG_WARN(terminal line editing will not be supported)
fi

AC_CHECK_LIB([z], [deflate], [], [AC_MSG_ERROR(zlib not found)])

# fst/fstapi.c can use pthread to write FST in parallel if HAVE_LIBPTHREAD
# and FST_WRITER_PARALLEL is defined.
# FIXME: -lpthread may be in LLVM_LDFLAGS already.
AC_ARG_ENABLE([fst_pthread],
  [AS_HELP_STRING([--enable-fst-pthread],
    [Use pthread to write FST in parallel])],
  [enable_fst_pthread=$enableval],
  [enable_fst_pthread=no])
if test x$enable_fst_pthread = xyes ; then
  AX_PTHREAD([], [AC_MSG_ERROR([pthread not found])])
  if test x$ax_pthread_ok = xyes ; then
    AC_DEFINE_UNQUOTED([HAVE_LIBPTHREAD], [1],
      [Preprequisite definition of GTKWave for parallel FST writer])
    AC_DEFINE_UNQUOTED([FST_WRITER_PARALLEL], [1],
      [Internal definition of GTKWave for parallel FST writer])
  fi
fi

AM_CONDITIONAL([FST_WRITER_PARALLEL], [test x$ax_pthread_ok = xyes])

# fst/fstapi.c can use Judy instead of builtin Jenkins if _WAVE_HAVE_JUDY is defined.
AC_ARG_ENABLE([fst_judy],
  [AS_HELP_STRING([--enable-fst-judy],
    [Use Judy instead of builtin Jenkins for FST])],
  [enable_fst_judy=$enableval],
  [enable_fst_judy=no])
if test x$enable_fst_judy = xyes ; then
  AC_CHECK_LIB([Judy], [JudyHSIns], [], [AC_MSG_ERROR(Judy not found)])
  AC_DEFINE_UNQUOTED([_WAVE_HAVE_JUDY], [1], [Internal definition of GTKWave for Judy])
fi

AX_LLVM_C([engine bitreader bitwriter ipo linker])
AM_CONDITIONAL([HAVE_LLVM], [test ! x$ax_cv_llvm = xyes])
AM_CONDITIONAL([FORCE_CXX_LINK], [test ! x$ax_cv_llvm_shared = xyes])

PKG_CHECK_EXISTS([check],
  [PKG_CHECK_MODULES([CHECK], [check >= 0.9.4], [], [])],
  [AC_MSG_WARN(libcheck not found - unit tests will not run)])

AX_DEFINE_DIR([DATADIR], [datadir/nvc], [Installation data directory])
AX_DEFINE_DIR([LIBDIR], [libdir], [Installation library directory])
AX_DEFINE_DIR([TESTDIR], [srcdir/test], [Location of testcases])

# CC may constain unwanted -std=... option.
AC_DEFINE_UNQUOTED([SYSTEM_CC], ["$(which ${CC%% *})"], [System compiler])

AC_ARG_ENABLE([native],
  [AS_HELP_STRING([--enable-native],
    [Enable native code generation])],
  [enable_native=$enableval],
  [enable_native=yes])

if test x$enable_native = xyes ; then
  AC_DEFINE_UNQUOTED([ENABLE_NATIVE], [1], [Enable native code generation])
  case $host_os in
    *cygwin*) implib_required=yes ;;
  esac
  if test x$implib_required = xyes ; then
    AC_DEFINE_UNQUOTED([IMPLIB_REQUIRED], [1], [Import library required])
  fi
fi

AM_CONDITIONAL([ENABLE_NATIVE], [test x$enable_native = xyes])
AM_CONDITIONAL([IMPLIB_REQUIRED], [test x$implib_required = xyes])

AC_ARG_ENABLE([vhpi],
  [AS_HELP_STRING([--enable-vhpi],
    [Enable VHPI plugins])],
  [enable_vhpi=$enableval],
  [enable_vhpi=no])

AM_CONDITIONAL([ENABLE_VHPI], [test x$enable_vhpi = xyes])

if test x$enable_vhpi = xyes; then
  AC_DEFINE_UNQUOTED([ENABLE_VHPI], [1], [VHPI is enabled])

  case $host_os in
    darwin*)
      VHPI_LDFLAGS="-flat_namespace -undefined dynamic_lookup"
      ;;
  esac
  AC_SUBST(VHPI_LDFLAGS)
fi

AC_ARG_ENABLE([werror],
  [AS_HELP_STRING([--enable-werror], [Treat warnings as errors])],
  [enable_werror=$enableval]
  [])
if test x$enable_werror = xyes ; then
  WERROR_CFLAGS="-Werror"
fi
AC_SUBST(WERROR_CFLAGS)

# AC_PC_FROM_UCONTEXT doesn't accept []-quoted argument.
AC_PC_FROM_UCONTEXT(AC_MSG_WARN(no way to access program counter - stack trace is disabled))
if test x$pc_field_found = xfalse ; then
  AC_DEFINE_UNQUOTED([NO_STACK_TRACE], [1], [Disable stack trace support])
fi

# Turn on silent make when using the GNU version
AX_CHECK_GNU_MAKE
if ! test x$_cv_gnu_make_command = x ; then
  MAKEFLAGS='$(if $(V),,-s)'
fi
AC_SUBST(MAKEFLAGS)

# Optional code coverage statistics
AC_ARG_ENABLE([gcov],
  [AS_HELP_STRING([--enable-gcov],
    [Instrument compiler with gcov (for development only)])],
  [enable_gcov=$enableval],
  [])
if test x$enable_gcov = xyes ; then
  COV_CFLAGS="--coverage --no-inline -Og"
  COV_LDFLAGS="--coverage"
  AC_MSG_WARN([code coverage with gcov is for use in development only])
fi
AC_SUBST(COV_CFLAGS)
AC_SUBST(COV_LDFLAGS)
AM_CONDITIONAL([ENABLE_GCOV], [test x$enable_gcov = xyes])

AC_CHECK_PROG([RONN], [ronn], [1])
AM_CONDITIONAL([HAVE_RONN], [test -n "$RONN"])

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([Makefile])
AC_OUTPUT
