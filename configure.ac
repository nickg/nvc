AC_INIT([nvc], [1.10-devel],
        [https://github.com/nickg/nvc/issues],
        [nvc],
        [https://www.nickg.me.uk/nvc/])
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

AM_INIT_AUTOMAKE([1.11 -Wall -Wno-portability color-tests
                  foreign subdir-objects] serial_tests)
AM_SILENT_RULES([yes])

AC_CONFIG_MACRO_DIR([m4])

AS_IF([test -d ${srcdir}/.git],
      [enable_maintainer_mode="yes"
       AC_DEFINE([HAVE_GIT_SHA], [1], [Have Git commit hash])],
      [enable_maintainer_mode="no"])
AM_MAINTAINER_MODE

AM_CONDITIONAL([ENABLE_GIT_SHA], [test -d ${srcdir}/.git])

FORCE_OUT_OF_TREE

AC_USE_SYSTEM_EXTENSIONS

AC_PROG_CC
AC_PROG_CXX
AC_PROG_INSTALL
AC_PROG_MKDIR_P
AC_PROG_LEX([noyywrap])
AC_PROG_YACC
AC_EXEEXT

AC_C_INLINE
AC_C_RESTRICT

AC_CHECK_HEADERS([sys/ptrace.h sys/prctl.h])
AC_CHECK_FUNCS([tcgetwinsize memmem strcasestr getline fseeko])
AC_CHECK_FUNCS([ftello fpurge __fpurge strchrnul strndup])

AC_CHECK_MEMBERS([struct stat.st_mtimespec.tv_nsec])
AC_CHECK_MEMBERS([struct stat.st_mtim.tv_nsec])

AC_CHECK_HEADERS([ucontext.h])
AC_CHECK_HEADERS([sys/ucontext.h])
AC_CHECK_HEADERS([stdio_ext.h])

AS_IF([test "$ac_cv_header_ucontext_h" = yes ],
      [AC_MSG_CHECKING([for getcontext])
       AC_LINK_IFELSE(
         [AC_LANG_PROGRAM([#include <ucontext.h>],
                          [ucontext_t uc; getcontext(&uc);])],
         [AC_MSG_RESULT([yes])
          AC_DEFINE([HAVE_GETCONTEXT], [1], [Defined if getcontext is available])],
         [AC_MSG_RESULT([no])])])

case $host_os in
  # Darwin does not allow empty archives
  darwin*) ac_cv_func_memmem=no ;;
esac
AM_CONDITIONAL([GNULIB_MEMMEM], [test "x$ac_cv_func_memmem" != xyes])
AM_CONDITIONAL([GNULIB_STRCASESTR], [test "x$ac_cv_func_strcasestr" != xyes])
AM_CONDITIONAL([GNULIB_GETLINE], [test "x$ac_cv_func_getline" != xyes])
AM_CONDITIONAL([GNULIB_STRCHRNUL], [test "x$ac_cv_func_strchrnul" != xyes])
AM_CONDITIONAL([GNULIB_STRNDUP], [test "x$ac_cv_func_strndup" != xyes])

EXTRA_CFLAGS=
EXTRA_LDFLAGS=

# Need to prevent GCC calling strndup builtin if using gnulib version
if test "x$ac_cv_func_strndup" != xyes; then
  AX_CHECK_COMPILE_FLAG([-fno-builtin-strndup],
                        [AS_VAR_APPEND([EXTRA_CFLAGS], [-fno-builtin-strndup])])
fi

# Prefer linking with Mold if available
AX_CHECK_LINK_FLAG(
  [-fuse-ld=mold],
  [AS_VAR_APPEND([EXTRA_CFLAGS], [-fuse-ld=mold])
   AX_CHECK_LINK_FLAG([-Wl,--undefined-version],
                      [AS_VAR_APPEND([EXTRA_LDFLAGS], [-Wl,--undefined-version])])])

CHECK_DYNAMIC_LIST(
  [EXPORT_LDFLAGS="-Wl,--dynamic-list=$srcdir/src/symbols.txt"],
  [AX_CHECK_LINK_FLAG([-rdynamic], [EXPORT_LDFLAGS="-rdynamic"])])
AC_SUBST(EXPORT_LDFLAGS)

AX_CHECK_COMPILE_FLAG([-fPIC],
  [PIC_FLAG="-fPIC"],
  [PIC_FLAG=""],
  [-Werror])
AC_SUBST(PIC_FLAG)

AX_CHECK_COMPILE_FLAG([-mavx2],
  [AC_DEFINE_UNQUOTED([HAVE_AVX2], [1], [Target supports AVX2 instructions])],
  [],
  [-Werror])

AX_GCC_FUNC_ATTRIBUTE([returns_nonnull])

AC_ARG_ENABLE([verilog],
  [AS_HELP_STRING([--enable-verilog], [Experimental Verilog support])],
  [enable_verilog=$enableval],
  [enable_verilog=no])

AM_CONDITIONAL([ENABLE_VERILOG], [test x$enable_verilog = xyes])

AS_IF([test x$enable_verilog = xyes],
      [AX_PROG_BISON([], [AC_MSG_ERROR(GNU Bison not found)])
       AC_DEFINE_UNQUOTED([ENABLE_VERILOG], [1], [Verilog support enabled])])
AX_PROG_FLEX([], [AC_MSG_ERROR(Flex not found)])

case $host_os in
  *cygwin*|msys*|mingw32*)
    LIBS="$LIBS -ldbghelp"    # For StackTrace64
    DIR_SEP=\\\\
    pathprog="cygpath -m"
    ;;
  *)
    AC_SEARCH_LIBS([dlopen], [dl dld], [], [
      AC_MSG_ERROR([unable to find the dlopen() function])
    ])
    DIR_SEP=/
    pathprog="echo"
    ;;
esac

AC_PATH_PROG([sh_path], ["sh"], ["/bin/sh"])
AC_DEFINE_UNQUOTED([SH_PATH], ["`$pathprog $sh_path`"], [Path to POSIX shell])

AC_PATH_PROG([diff_path], ["diff"], ["/usr/bin/diff"])
AC_DEFINE_UNQUOTED([DIFF_PATH], ["`$pathprog $diff_path`"], [Path to diff program])

AC_DEFINE_UNQUOTED([DIR_SEP], ["$DIR_SEP"], [Directory separator])
AC_DEFINE_UNQUOTED([EXEEXT], ["$EXEEXT"], [Executable file extension])

AC_SEARCH_LIBS([pow], [m], [], [
  AC_MSG_ERROR([unable to find the pow() function])
])

AC_CHECK_LIB([z], [deflate], [], [AC_MSG_ERROR(zlib not found)])

AC_DEFINE_UNQUOTED([FST_REMOVE_DUPLICATE_VC], [1], [Enable FST glitch removal])

AX_PTHREAD([], [AC_MSG_ERROR([pthread not found])])

AC_ARG_ENABLE([llvm],
  [AS_HELP_STRING([--enable-llvm], [Build LLVM code generator])],
  [enable_llvm=$enableval],
  [enable_llvm=yes])

AM_CONDITIONAL([ENABLE_LLVM], [test x$enable_llvm != xno])

if test "$enable_llvm" != "no"; then
  AC_DEFINE_UNQUOTED([ENABLE_LLVM], [1], [LLVM code generator enabled])
  AX_LLVM_C([engine passes ipo linker native])
else
  AM_CONDITIONAL([LLVM_STATIC], [false])
fi

AC_ARG_ENABLE([jit],
  [AS_HELP_STRING([--enable-jit], [Enable JIT complilation with LLVM (EXPERIMENTAL)])],
  [enable_llvm=$enableval],
  [enable_llvm=no])

if test "$enable_jit" = "yes"; then
  if test "$enable_llvm" != "yes"; then
    AC_MSG_ERROR([JIT compilation depends on LLVM])
  fi

  AC_DEFINE_UNQUOTED([ENABLE_JIT], [1], [JIT compilation enabled])
fi

PKG_CHECK_MODULES([check], [check >= 0.9.4], [],
                  [AC_MSG_WARN(libcheck not found - unit tests will not run)])

PKG_CHECK_MODULES([capstone], [capstone >= 4.0],
                  [AC_DEFINE_UNQUOTED([HAVE_CAPSTONE], [1],
                                      [Have capstone for diassembly])],
                  [true])

PKG_CHECK_MODULES([libffi], [libffi >= 3.0])

PKG_CHECK_MODULES([libzstd], [libzstd >= 1.4])

AC_ARG_ENABLE([tcl],
  [AS_HELP_STRING([--enable-tcl], [Build TCL interface])],
  [enable_tcl=$enableval],
  [enable_tcl=no])

AS_IF([test x$enable_tcl = xyes],
      [AC_DEFINE_UNQUOTED([ENABLE_TCL], [1], [TCL shell enabled])

       found_tcl=no
       for pkg in tcl8.6 tcl86 tcl; do
         PKG_CHECK_MODULES([TCL], [$pkg >= 8.6.0],
                           [found_tcl=yes], [true])
       done
       AS_IF([test x$found_tcl = xno],
             [AC_MSG_ERROR([TCL library not found, try installing tcl-dev])])

       AX_LIB_READLINE
       if test "x$ax_cv_lib_readline" = xno ; then
         AC_MSG_ERROR([readline or a compatible library is required when TCL is enabled])
       fi])

AM_CONDITIONAL([ENABLE_TCL], [test x$enable_tcl = xyes])

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

AX_DEFINE_DIR([PREFIX], [prefix], [Installation prefix])
AX_DEFINE_DIR([LIBDIR], [libdir/nvc], [Installation library directory])
AX_DEFINE_DIR([LIBEXECDIR], [libexecdir/nvc], [Location of internal programs])
AX_DEFINE_DIR([TESTDIR], [srcdir/test], [Location of testcases])

case $host_cpu in
  x86_64|amd64)
    AC_DEFINE_UNQUOTED([ARCH_X86_64], [1], [Host is x86_64])
    ;;
  aarch64)
    AC_DEFINE_UNQUOTED([ARCH_ARM64], [1], [Host is arm64])
    ;;
esac

AM_CONDITIONAL([ARCH_X86_64],
               [test x$host_cpu = xx86_64 -o x$host_cpu = xamd64])
AM_CONDITIONAL([ARCH_ARM64], [test x$host_cpu = xaarch64])

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

case $host_os in
  *cygwin*|msys*|mingw32*)
    # Do not hard-code full path on Windows
    system_cc="$(basename $system_cc)"
    ;;
esac

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

    AX_CHECK_LINK_FLAG(
      [-Wl,-no_fixup_chains],
      [AC_DEFINE([HAVE_NO_FIXUP_CHAINS], [1],
                 [Defined if linker supports -no_fixup_chains])
       AX_APPEND_FLAG([-Wl,-no_fixup_chains], [VHPI_LDFLAGS])])
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

AC_ARG_ENABLE([frame-pointer],
  [AS_HELP_STRING([--enable-frame-pointer],
                  [Preserve frame pointer for profiling])],
  [AX_APPEND_FLAG([-fno-omit-frame-pointer], [EXTRA_CFLAGS])
   preserve_frame_pointer=$enableval],
  [preserve_frame_pointer=no])

if test x$preserve_frame_pointer = xyes ; then
  AC_DEFINE_UNQUOTED([PRESERVE_FRAME_POINTER], [1],
                     [Preserve frame pointer for profiling])
fi

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

AC_ARG_ENABLE([ubsan],
  [AS_HELP_STRING([--enable-ubsan], [Enable undefined behaviour sanitiser])],
  [if test x$enableval = xyes; then
     EXTRA_CFLAGS="$EXTRA_CFLAGS -fsanitize=undefined"
     EXTRA_LDFLAGS="$EXTRA_LDFLAGS -fsanitize=undefined"
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

AC_ARG_ENABLE([default-paths],
  [AS_HELP_STRING([--disable-default-paths],
                  [Disable default library search paths])],
  [enable_default_paths=$enableval],
  [enable_default_paths=yes])

if test x$enable_default_paths != xno ; then
  AC_DEFINE_UNQUOTED([ENABLE_DEFAULT_PATHS], [1],
                     [Enable default library search paths])
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

AC_ARG_WITH([bash-completion],
  AS_HELP_STRING(
    [--with-bash-completion[=PATH]],
    [Install the bash auto-completion script in this directory.]),
  [],
  [with_bash_completion=yes])

if test "x$with_bash_completion" = "xyes"; then
  BASH_COMPLETION_DIR="$datadir/bash-completion/completions"
else
  BASH_COMPLETION_DIR="$with_bash_completion"
fi

AC_SUBST([BASH_COMPLETION_DIR])
AM_CONDITIONAL([ENABLE_BASH_COMPLETION],
               [test "x$with_bash_completion" != "xno"])

# Turn on silent make when using the GNU version
AX_CHECK_GNU_MAKE
if test x$_cv_have_gnu_make = xyes ; then
  MAKEFLAGS='$(if $(V),,-s)'
fi

AC_ARG_ENABLE([parallel_make],
              [AS_HELP_STRING([--enable-parallel-make],
                              [Enable make -j by default])],
              [parallel_make=$enableval],
              [parallel_make=no])

# Build with make -j by default but only if requested
if test x$parallel_make = xyes; then
  AC_MSG_CHECKING([whether to enable parallel make by default])
  AX_COMPARE_VERSION([$ax_check_gnu_make_version], [ge], [4,0],
                     [AC_MSG_RESULT([yes])
                      AX_APPEND_FLAG([-j], [MAKEFLAGS])],
                     [AC_MSG_RESULT([no])])
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
      [AC_PATH_PROG([MANDOC], [mandoc])])

AM_CONDITIONAL([HAVE_MANDOC], [test -n "$MANDOC"])

# Work around automake setting ARFLAGS=cru by default which produces a
# warning on modern systems
ARFLAGS="${ARFLAGS:-cr}"
AC_SUBST(ARFLAGS)

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([Makefile])
AC_OUTPUT
