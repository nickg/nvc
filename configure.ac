AC_INIT([nvc], [1.6-devel],
        [Nick Gasson <nick@nickg.me.uk>],
        [nvc])
AC_PREREQ([2.63])

AC_CANONICAL_HOST
AC_CANONICAL_TARGET

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

AC_PROG_CC
AC_PROG_CC_C99
AC_PROG_CXX
AC_PROG_INSTALL
AC_PROG_MKDIR_P
AC_PROG_LEX
AC_PROG_RANLIB
AC_EXEEXT

AC_C_INLINE
AC_C_RESTRICT

AC_CHECK_HEADERS([sys/ptrace.h sys/prctl.h])

AC_CHECK_MEMBERS([struct stat.st_mtimespec.tv_nsec])
AC_CHECK_MEMBERS([struct stat.st_mtim.tv_nsec])

EXTRA_CFLAGS=
EXTRA_LDFLAGS=

AX_CHECK_LINK_FLAG([-rdynamic],
                   [EXTRA_LDFLAGS="$EXTRA_LDFLAGS -rdynamic"])

AX_CHECK_COMPILE_FLAG([-fPIC],
  [PIC_FLAG="-fPIC"],
  [PIC_FLAG=""],
  [-Werror])
AC_SUBST(PIC_FLAG)

AX_GCC_FUNC_ATTRIBUTE([returns_nonnull])

AX_PROG_FLEX([], [AC_MSG_ERROR(Flex not found)])

case $host_os in
  *cygwin*|msys*|mingw32*)
    # LLVM on Windows needs libffi and curses
    AX_WITH_CURSES
    if test "x$ax_cv_curses" != xyes; then
      AC_MSG_ERROR([LLVM on Windows requires curses])
    fi

    AC_SEARCH_LIBS([ffi_call], [ffi], [],
      [AC_MSG_ERROR(LLVM on Windows requires libffi)], [])
    ;;
esac

case $host_os in
  msys*|mingw32*)
    PATH_SEP=\\\\
    ;;
  *)
    PATH_SEP=/
    ;;
esac

AC_DEFINE_UNQUOTED([PATH_SEP], ["$PATH_SEP"], [Path separator])
AC_DEFINE_UNQUOTED([EXEEXT], ["$EXEEXT"], [Executable file extension])

case $host_os in
  msys*|mingw32*)
    ;;
  *)
    AC_SEARCH_LIBS([dlopen], [dl dld], [], [
      AC_MSG_ERROR([unable to find the dlopen() function])
    ])
    ;;
esac

AC_SEARCH_LIBS([pow], [m], [], [
  AC_MSG_ERROR([unable to find the pow() function])
])

AC_CHECK_LIB([z], [deflate], [], [AC_MSG_ERROR(zlib not found)])

AC_ARG_ENABLE([fst_debug],
  [AS_HELP_STRING([--enable-fst-debug],
    [Enable FST debugging (for development only)])],
  [enable_fst_debug=$enableval],
  [enable_fst_debug=no])
if test x$enable_fst_debug = xyes ; then
  AC_DEFINE_UNQUOTED([FST_DEBUG], [1],
    [Enable FST debugging (for development only)])
fi

AC_ARG_ENABLE([fst_glitch_removal],
  [AS_HELP_STRING([--enable-fst-glitch-removal],
    [Enable FST glitch removal (has performance impact)])],
  [enable_fst_glitch_removal=$enableval],
  [enable_fst_glitch_removal=no])
if test x$enable_fst_glitch_removal = xyes ; then
  AC_DEFINE_UNQUOTED([FST_REMOVE_DUPLICATE_VC], [1],
    [Enable FST glitch removal (has performance impact)])
fi

# thirdparty/fstapi.c can use pthread to write FST in parallel if HAVE_LIBPTHREAD
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

# thirdparty/fstapi.c can use Judy instead of builtin Jenkins if _WAVE_HAVE_JUDY is defined.
AC_ARG_ENABLE([fst_judy],
  [AS_HELP_STRING([--enable-fst-judy],
    [Use Judy instead of builtin Jenkins for FST])],
  [enable_fst_judy=$enableval],
  [enable_fst_judy=no])
if test x$enable_fst_judy = xyes ; then
  AC_CHECK_LIB([Judy], [JudyHSIns], [], [AC_MSG_ERROR(Judy not found)])
  AC_DEFINE_UNQUOTED([_WAVE_HAVE_JUDY], [1], [Internal definition of GTKWave for Judy])
fi

AX_LLVM_C([engine passes ipo linker native])

PKG_CHECK_MODULES([CHECK], [check >= 0.9.4], [],
                  [AC_MSG_WARN(libcheck not found - unit tests will not run)])

debug_backend=
AC_CHECK_HEADER([unwind.h],
  [
    AC_DEFINE_UNQUOTED([HAVE_UNWIND_H], [1], [Have unwind.h])
    debug_backend=unwind

    AC_SEARCH_LIBS([_Unwind_Backtrace], [c++abi],
      [
        # libc++-abi needs to link -lpthread on OpenBSD
        if test "$ac_cv_search__Unwind_Backtrace" = "-lc++abi"; then
          AX_PTHREAD([LIBS="$PTHREAD_LIBS $LIBS"],
                     [AC_MSG_ERROR([pthread not found])])
        fi
      ],
      [AC_MSG_ERROR(cannot find library for _Unwind_Backtrace)],
      [-lpthread])

    PKG_CHECK_MODULES([libdw], [libdw >= 0.159],
      [AC_DEFINE_UNQUOTED([HAVE_LIBDW], [1], [Have libdw for stack traces])
       debug_backend=libdw],
      [true]) # Do not error if libdw not installed

    # libdw and libdwarf conflict so take care not to add both to LIBS
    if test "$debug_backend" != "libdw"; then
      AC_CHECK_HEADER([libdwarf/libdwarf.h],
        [AC_SEARCH_LIBS([elf_version], [elf], [],
                        [AC_MSG_ERROR(cannot find libelf)])
         AC_SEARCH_LIBS([dwarf_init], [dwarf],
                        [AC_DEFINE_UNQUOTED([HAVE_LIBDWARF], [1],
                                            [Have libdwarf for stack traces])
                         debug_backend=libdwarf],
                        [AC_MSG_WARN(cannot link against libdwarf)], [])])
    fi
  ])

case $host_os in
  *cygwin*|msys*|mingw32*)
    debug_backend=win32
    ;;
esac

if test -z "$debug_backend"; then
  AX_EXECINFO
  AC_MSG_NOTICE(libdw or libdwarf not found - using basic stack traces)
  debug_backend=execinfo
fi

AX_DEFINE_DIR([DATADIR], [datadir/nvc], [Installation data directory])
AX_DEFINE_DIR([LIBDIR], [libdir], [Installation library directory])
AX_DEFINE_DIR([TESTDIR], [srcdir/test], [Location of testcases])

# Prefer calling the linker directy to using CC
linker_path=
case $host_os in
  linux*|openbsd*)
    AC_PATH_TARGET_TOOL([linker_path], [ld])
    ;;
esac

if test -n "$linker_path"; then
   AC_DEFINE_UNQUOTED([LINKER_PATH], ["$linker_path"], [System linker])
fi

# CC may constain unwanted -std=... option.
cc_bare="$(which ${CC%% *})"
case $host_os in
  *cygwin*|msys*|mingw32*)
    cc_bare="$(cygpath -w $cc_bare | sed  's|\\|\\\\|g')"
    ;;
esac
AC_ARG_WITH([system-cc],
            [AS_HELP_STRING([--with-system-cc=PATH],
                            [Path to system compiler to be used for linking])],
            [system_cc="$withval"],
            [system_cc="$cc_bare"])
AC_DEFINE_UNQUOTED([SYSTEM_CC], ["$system_cc"], [System compiler])

case $host_os in
  *cygwin*|msys*|mingw32*)
    implib_required=yes
    DLL_EXT=dll
    LIB_EXT=obj
    ;;
  *)
    DLL_EXT=so
    LIB_EXT=a
    ;;
esac
if test x$implib_required = xyes ; then
  AC_DEFINE_UNQUOTED([IMPLIB_REQUIRED], [1], [Import library required])
fi
AC_DEFINE_UNQUOTED([DLL_EXT], ["$DLL_EXT"], [Dynamic library extension])
AC_SUBST(DLL_EXT)
AC_SUBST(LIB_EXT)
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
  [EXTRA_CFLAGS="$EXTRA_CFLAGS -Werror"])

AC_ARG_ENABLE([ubsan],
  [AS_HELP_STRING([--enable-ubsan], [Enable undefined behaviour sanitiser])],
  [if test x$enableval = xyes; then
     EXTRA_CFLAGS="$EXTRA_CFLAGS -fsanitize=undefined"
     EXTRA_LDFLAGS="$EXTRA_LDFLAGS -fsanitize=undefined"
   fi])

case $host_os in
  msys*|mingw32*)
    LIBS="$LIBS -lDbgHelp"    # For StackTrace64
    ;;
  *)
    AC_PC_FROM_UCONTEXT(
      AC_MSG_WARN([no way to access program counter]))
    ;;
esac

# Turn on silent make when using the GNU version
AX_CHECK_GNU_MAKE
if test x$_cv_have_gnu_make = xyes ; then
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
  EXTRA_CFLAGS="$EXTRA_CFLAGS --coverage -fno-inline -Og"
  EXTRA_LDFLAGS="$EXTRA_LDFLAGS --coverage"
  AC_MSG_WARN([code coverage with gcov is for use in development only])
fi
AM_CONDITIONAL([ENABLE_GCOV], [test x$enable_gcov = xyes])

AC_SUBST(EXTRA_CFLAGS)
AC_SUBST(EXTRA_LDFLAGS)

AC_PATH_PROG([MANDOC], [mandoc])
AM_CONDITIONAL([HAVE_MANDOC], [test -n "$MANDOC"])

# Work around automake setting ARFLAGS=cru by default which produces a
# warning on modern systems
ARFLAGS="${ARFLAGS:-cr}"
AC_SUBST(ARFLAGS)

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([Makefile])
AC_OUTPUT
