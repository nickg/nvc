# -*- mode: sh -*-
# ===========================================================================
#          http://www.gnu.org/software/autoconf-archive/ax_llvm.html
# ===========================================================================
#
# SYNOPSIS
#
#   AX_LLVM([llvm-libs])
#
# DESCRIPTION
#
#   Test for the existance of llvm, and make sure that it can be linked with
#   the llvm-libs argument that is passed on to llvm-config i.e.:
#
#     llvm --libs <llvm-libs>
#
#   llvm-config will also include any libraries that are depended apon.
#
# LICENSE
#
#   Copyright (c) 2008 Andy Kitchen <agimbleinthewabe@gmail.com>
#
#   Copying and distribution of this file, with or without modification, are
#   permitted in any medium without royalty provided the copyright notice
#   and this notice are preserved. This file is offered as-is, without any
#   warranty.

#
# Modified by Nick Gasson to work with the C bindings
#

#serial 12

AC_DEFUN([AX_LLVM_C], [
  AC_ARG_WITH([llvm],
    AS_HELP_STRING(
      [--with-llvm=PATH],
      [Use a specific version of llvm-config]),
    [
      if test "$withval" != "no"; then
        ac_llvm_config_path="$withval"
      fi
    ],
    [])

  llvm_maybe_static=no
  case $host_os in
    darwin*)
      ac_llvm_link_mode=""
      llvm_maybe_static=yes
      ;;
    *)
      ac_llvm_link_mode="--link-shared"
      ;;
  esac

  AC_ARG_ENABLE([static_llvm],
                [AS_HELP_STRING([--enable-static-llvm],
                                [Link to static LLVM libraries (default shared)])],
                [
                  if test x$enableval = xyes; then
                    ac_llvm_link_mode="--link-static"
                    llvm_maybe_static=yes
                  fi
                ])

  succeeded=no
  if test -z "$ac_llvm_config_path"; then
    ac_llvm_config_path=`which llvm-config`
  fi

  if test -e "$ac_llvm_config_path"; then
    ac_llvm_config="$ac_llvm_config_path $ac_llvm_link_mode"

    LLVM_VERSION="$($ac_llvm_config --version)"
    llvm_ver_num=`echo $LLVM_VERSION | sed 's/\(@<:@0-9@:>@\{1,\}\)\.\(@<:@0-9@:>@\{1,\}\).*/\1\2/'`

    LLVM_CFLAGS=`$ac_llvm_config --cflags`
    LLVM_CXXFLAGS=`$ac_llvm_config --cxxflags`
    LLVM_LDFLAGS="$($ac_llvm_config --ldflags | sed 's|\\|\\\\|g')"
    LLVM_SYSLIBS="$($ac_llvm_config --system-libs)"
    LLVM_LIBS="$($ac_llvm_config --libs $1) $LLVM_SYSLIBS"
    LLVM_CONFIG_BINDIR="$($ac_llvm_config $ac_llvm_config_flags --bindir | sed 's|\\|\\\\|g')"
    LLVM_LIBDIR="$($ac_llvm_config --libdir | sed 's|\\|\\\\|g')"

    if test "$llvm_ver_num" -lt "70"; then
      AC_MSG_ERROR([LLVM version 7.0 or later required])
    fi

    if test "$llvm_ver_num" -ge "80"; then
      AC_DEFINE_UNQUOTED(LLVM_HAVE_BUILD_MEMSET, [1],
                         [Have LLVMBuildMemSet])
    fi

    if test "$llvm_ver_num" -ge "160"; then
      AC_MSG_ERROR([LLVM version 16.0 or later not yet supported])
    fi

    if test "$llvm_ver_num" -ge "100"; then
      AC_DEFINE_UNQUOTED(LLVM_HAVE_DI_SCOPE_GET_FILE, [1],
                         [Have LLVMDIScopeGetFile])
      AC_DEFINE_UNQUOTED(LLVM_HAVE_SET_CURRENT_DEBUG_LOCATION_2, [1],
                         [Have LLVMSetCurrentDebugLocation2])
    fi

    if test "$llvm_ver_num" -ge "110"; then
      AC_DEFINE_UNQUOTED(LLVM_CREATE_CU_HAS_SYSROOT, [1],
                         [LLVMDIBuilderCreateCompileUnit has SysRoot parameter])
    fi

    if test "$llvm_ver_num" -ge "150"; then
      AC_DEFINE_UNQUOTED(LLVM_HAS_OPAQUE_POINTERS, [1],
                         [LLVM uses opaque pointers by default])
    fi

    if test "$llvm_ver_num" -ge "150"; then
      AC_DEFINE_UNQUOTED(LLVM_UWTABLE_HAS_ARGUMENT, [1],
                         [LLVM uwtable attribute takes an argument])
    fi

    LLVM_OBJ_EXT="o"
    case $host_os in
      *cygwin*|msys*|mingw32*)
        LLVM_OBJ_EXT="obj"
        ;;
    esac
    AC_DEFINE_UNQUOTED(LLVM_OBJ_EXT, ["$LLVM_OBJ_EXT"],
                       [LLVM object file extension])

    AC_REQUIRE([AC_PROG_CXX])

    CFLAGS_SAVED="$CFLAGS"
    CFLAGS="$CFLAGS $LLVM_CFLAGS"
    export CFLAGS

    CXXFLAGS_SAVED="$CXXFLAGS"
    CXXFLAGS="$CXXFLAGS $LLVM_CXXFLAGS"
    export CXXFLAGS

    LDFLAGS_SAVED="$LDFLAGS"
    LDFLAGS="$LDFLAGS $LLVM_LDFLAGS"
    export LDFLAGS

    LIBS_SAVED="$LIBS"
    LIBS="$LIBS $LLVM_LIBS"
    export LIBS

    AC_CACHE_CHECK([for LLVM ([$1])],
                   ax_cv_llvm,
                   [AC_LANG_PUSH([C++])
                    AC_LINK_IFELSE(
                      [AC_LANG_PROGRAM(
                         [[@%:@include <llvm-c/Core.h>]],
                         [[LLVMModuleCreateWithName("test"); return 0;]])],
                      ax_cv_llvm=yes,
                      ax_cv_llvm=no)
                    AC_LANG_POP([C++])])

    if test "x$ax_cv_llvm" = "xyes"; then
      succeeded=yes
    fi

    CFLAGS="$CFLAGS_SAVED"
    CXXFLAGS="$CXXFLAGS_SAVED"
    LDFLAGS="$LDFLAGS_SAVED"
    LIBS="$LIBS_SAVED"
  else
    succeeded=no
  fi

  if test "$succeeded" != "yes" ; then
    AC_MSG_ERROR([Could not detect the LLVM libraries. Make sure that llvm-config is on your path or specified by --with-llvm.])
  else
    AC_SUBST(LLVM_CFLAGS)
    AC_SUBST(LLVM_LDFLAGS)
    AC_SUBST(LLVM_LIBS)
    AM_CONDITIONAL([LLVM_STATIC], [test x$llvm_maybe_static = xyes])
    AC_DEFINE(HAVE_LLVM,,[Defined if LLVM is available])
    AC_DEFINE_UNQUOTED(LLVM_VERSION,["$LLVM_VERSION"],[Version of LLVM installed])
    AC_DEFINE_UNQUOTED(LLVM_CONFIG_BINDIR,["$LLVM_CONFIG_BINDIR"],[Location of LLVM binaries])
  fi
])
