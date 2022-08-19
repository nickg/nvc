AC_INIT([nvc], [1.7.1],
        [Nick Gasson <nick@nickg.me.uk>],
        [nvc],
        [https://github.com/nickg/nvc])
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

AS_IF([test -d ${srcdir}/.git],
      [enable_maintainer_mode="yes"],
      [enable_maintainer_mode="no"])
AM_MAINTAINER_MODE

FORCE_OUT_OF_TREE

AC_PROG_CC
AC_PROG_CXX
AC_PROG_INSTALL
AC_PROG_MKDIR_P
AC_PROG_LEX([noyywrap])
AC_EXEEXT

AC_C_INLINE
AC_C_RESTRICT

AC_CHECK_HEADERS([sys/ptrace.h sys/prctl.h])
AC_CHECK_FUNCS([tcgetwinsize])

AC_CHECK_MEMBERS([struct stat.st_mtimespec.tv_nsec])
AC_CHECK_MEMBERS([struct stat.st_mtim.tv_nsec])

AC_CHECK_HEADERS([ucontext.h])
AC_CHECK_HEADERS([sys/ucontext.h])

AS_IF([test "$ac_cv_header_ucontext_h" = yes ],
      [AC_MSG_CHECKING([for getcontext])
       AC_LINK_IFELSE(
         [AC_LANG_PROGRAM([#include <ucontext.h>],
                          [ucontext_t uc; getcontext(&uc);])],
         [AC_MSG_RESULT([yes])
          AC_DEFINE([HAVE_GETCONTEXT], [1], [Defined if getcontext is available])],
         [AC_MSG_RESULT([no])])])

EXTRA_CFLAGS=
EXTRA_LDFLAGS=

CHECK_DYNAMIC_LIST(
  [EXPORT_LDFLAGS="-Wl,--dynamic-list=$srcdir/src/symbols.txt"],
  [AX_CHECK_LINK_FLAG([-rdynamic], [EXPORT_LDFLAGS="-rdynamic"])])
AC_SUBST(EXPORT_LDFLAGS)

AX_CHECK_COMPILE_FLAG([-fPIC],
  [PIC_FLAG="-fPIC"],
  [PIC_FLAG=""],
  [-Werror])
AC_SUBST(PIC_FLAG)

AX_GCC_FUNC_ATTRIBUTE([returns_nonnull])

AX_PROG_FLEX([], [AC_MSG_ERROR(Flex not found)])

AC_PATH_PROG([sh_path], ["sh"], ["/bin/sh"])
AC_DEFINE_UNQUOTED([SH_PATH], ["$sh_path"], [Path to POSIX shell])

case $host_os in
  *cygwin*|msys*|mingw32*)
    # LLVM on Windows needs libffi and curses
    AX_WITH_CURSES
    if test "x$ax_cv_curses" != xyes; then
      AC_MSG_ERROR([LLVM on Windows requires curses])
    fi

    LIBS="$LIBS -lDbgHelp"    # For StackTrace64

    AC_SEARCH_LIBS([ffi_call], [ffi], [],
      [AC_MSG_ERROR(LLVM on Windows requires libffi)], [])

    AC_PATH_PROG([MINGW_BASH], ["bash"],
                 [AC_MSG_WARN([bash not found])])
    AC_DEFINE_UNQUOTED([BASH_PATH], ["$(cygpath -wm $MINGW_BASH)"],
                       [Path to Bash on Windows])
    ;;

  *)
    AC_SEARCH_LIBS([dlopen], [dl dld], [], [
      AC_MSG_ERROR([unable to find the dlopen() function])
    ])
    ;;
esac

case $host_os in
  msys*|mingw32*)
    DIR_SEP=\\\\
    ;;
  *)
    DIR_SEP=/
    ;;
esac

AC_DEFINE_UNQUOTED([DIR_SEP], ["$DIR_SEP"], [Directory separator])
AC_DEFINE_UNQUOTED([EXEEXT], ["$EXEEXT"], [Executable file extension])

AC_SEARCH_LIBS([pow], [m], [], [
  AC_MSG_ERROR([unable to find the pow() function])
])

AC_CHECK_LIB([z], [deflate], [], [AC_MSG_ERROR(zlib not found)])

AC_DEFINE_UNQUOTED([FST_REMOVE_DUPLICATE_VC], [1], [Enable FST glitch removal])

AX_PTHREAD([], [AC_MSG_ERROR([pthread not found])])

AX_LLVM_C([engine passes ipo linker native])

PKG_CHECK_MODULES([check], [check >= 0.9.4], [],
                  [AC_MSG_WARN(libcheck not found - unit tests will not run)])

AC_CHECK_HEADER([unwind.h], [],
  [AC_MSG_ERROR([required header unwind.h not found (try installing libunwind)])])

AC_SEARCH_LIBS([_Unwind_Backtrace], [c++abi],
  [
    # libc++-abi needs to link -lpthread on OpenBSD
    if test "$ac_cv_search__Unwind_Backtrace" = "-lc++abi"; then
      AX_PTHREAD([LIBS="$PTHREAD_LIBS $LIBS"],
                 [AC_MSG_ERROR([pthread not found])])
    fi
  ],
  [AC_MSG_ERROR([cannot find library for _Unwind_Backtrace])],
  [-lpthread])

case $host_os in
  linux*)
    PKG_CHECK_MODULES([libdw], [libdw >= 0.159],
      [AC_DEFINE_UNQUOTED([HAVE_LIBDW], [1], [Have libdw for stack traces])])
    ;;

  freebsd*|openbsd*)
    AC_CHECK_HEADERS([libdwarf/libdwarf.h libdwarf.h],
      [AC_SEARCH_LIBS([elf_version], [elf], [],
                      [AC_MSG_ERROR(cannot find libelf)])
       AC_SEARCH_LIBS([dwarf_init], [dwarf],
                      [AC_DEFINE_UNQUOTED([HAVE_LIBDWARF], [1],
                                          [Have libdwarf for stack traces])],
                      [AC_MSG_ERROR(cannot link against libdwarf)], [])
       break])
    if test -z "$ac_cv_search_dwarf_init"; then
      AC_MSG_ERROR([cannot find libdwarf.h])
    fi
    ;;
esac

AX_DEFINE_DIR([DATADIR], [datadir/nvc], [Installation data directory])
AX_DEFINE_DIR([LIBDIR], [libdir/nvc], [Installation library directory])
AX_DEFINE_DIR([LIBEXECDIR], [libexecdir/nvc], [Location of internal programs])
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
    cc_bare="$(cygpath -wm $cc_bare)"
    ;;
esac
AC_ARG_WITH([system-cc],
            [AS_HELP_STRING([--with-system-cc=PATH],
                            [Path to system compiler to be used for linking])],
            [system_cc="$withval"],
            [system_cc="$cc_bare"])

AC_MSG_CHECKING([if $system_cc executable exists])
AS_IF([test -x $system_cc],
      [AC_MSG_RESULT([yes])],
      [AC_MSG_RESULT([no])
       AC_PATH_PROG([system_cc], ["$system_cc"],
                    [AC_MSG_ERROR([system C compiler $system_cc not found])])])

AC_DEFINE_UNQUOTED([SYSTEM_CC], ["$system_cc"], [System compiler])

case $host_os in
  *cygwin*|msys*|mingw32*)
    implib_required=yes
    DLL_EXT=dll
    LIB_EXT=lib
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

case $host_os in
  darwin*)
    VHPI_LDFLAGS="-flat_namespace -undefined dynamic_lookup"
    ;;
esac
AC_SUBST(VHPI_LDFLAGS)

AC_ARG_ENABLE([vital],
  [AS_HELP_STRING([--enable-vital], [Build VITAL packages])],
  [enable_vital=$enableval],
  [enable_vital=yes])

AM_CONDITIONAL([ENABLE_VITAL], [test x$enable_vital = xyes])

AC_ARG_ENABLE([werror],
  [AS_HELP_STRING([--enable-werror], [Treat warnings as errors])],
  [EXTRA_CFLAGS="$EXTRA_CFLAGS -Werror"])

AC_ARG_ENABLE([ubsan],
  [AS_HELP_STRING([--enable-ubsan], [Enable undefined behaviour sanitiser])],
  [if test x$enableval = xyes; then
     EXTRA_CFLAGS="$EXTRA_CFLAGS -fsanitize=undefined"
     EXTRA_LDFLAGS="$EXTRA_LDFLAGS -fsanitize=undefined"
   fi])

AC_ARG_ENABLE([asan],
  [AS_HELP_STRING([--enable-sasn], [Enable address sanitiser])],
  [if test x$enableval = xyes; then
     EXTRA_CFLAGS="$EXTRA_CFLAGS -fsanitize=address -fno-omit-frame-pointer"
     EXTRA_LDFLAGS="$EXTRA_LDFLAGS -fsanitize=address -fno-omit-frame-pointer"
   fi])

AC_ARG_ENABLE([tsan],
  [AS_HELP_STRING([--enable-tsan], [Enable thread sanitiser])],
  [if test x$enableval = xyes; then
     EXTRA_CFLAGS="$EXTRA_CFLAGS -fsanitize=thread"
     EXTRA_LDFLAGS="$EXTRA_LDFLAGS -fsanitize=thread"
   fi])

AC_ARG_ENABLE([debug],
  [AS_HELP_STRING([--enable-debug],
                  [Enable extra debugging checks for development])],
  [enable_debug=$enableval],
  [enable_debug=no])

if test x$enable_debug != xyes ; then
  AC_DEFINE_UNQUOTED([NDEBUG], [1],
                     [Disable extra debugging checks for development])
fi

AC_ARG_ENABLE([lto],
  [AS_HELP_STRING([--enable-lto], [Enable link time optimisation])],
  [enable_lto=$enableval],
  [AS_IF([test x$enable_debug = xyes],
         [enable_lto=no],   # LTO off by default in debug builds
         [enable_lto=yes])])

LTO_FLAG=
if test x$enable_lto = xyes; then
  AX_CHECK_COMPILE_FLAG([-flto=auto], [LTO_FLAG="-flto=auto"], [], [-Werror])
  AX_CHECK_LINK_FLAG([$LTO_FLAG], [], [LTO_FLAG=], [-Werror])

  AX_COMPILER_VENDOR
  AS_CASE([$ax_cv_c_compiler_vendor],
          [gnu], [AC_CHECK_TOOLS([AR], [gcc-ar ar])
                  AC_CHECK_TOOLS([RANLIB], [gcc-ranlib ranlib])],
          [clang], [AC_CHECK_TOOLS([AR], [llvm-ar ar])
                    AC_CHECK_TOOLS([RANLIB], [llvm-ranlib ranlib])])
else
  AC_CHECK_TOOL([AR], [ar])
  AC_PROG_RANLIB
fi
AC_SUBST(LTO_FLAG)

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

AS_IF([test x$enable_maintainer_mode = xyes ],
      [AC_PATH_PROG([MANDOC], [mandoc])
       AC_PATH_PROG([PANDOC], [pandoc])])

AM_CONDITIONAL([HAVE_MANDOC], [test -n "$MANDOC"])
AM_CONDITIONAL([HAVE_PANDOC], [test -n "$PANDOC"])

# Work around automake setting ARFLAGS=cru by default which produces a
# warning on modern systems
ARFLAGS="${ARFLAGS:-cr}"
AC_SUBST(ARFLAGS)

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([Makefile])
AC_OUTPUT
