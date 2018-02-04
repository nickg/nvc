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
      [--with-llvm@<:@=DIR@:>@],
      [use llvm (default is yes) - it is possible to specify the root directory for llvm (optional)]),
      [
        if test "$withval" = "no"; then
            want_llvm="no"
        elif test "$withval" = "yes"; then
            want_llvm="yes"
            ac_llvm_config_path=`which llvm-config`
        else
            want_llvm="yes"
            ac_llvm_config_path="$withval"
        fi
      ],
      [want_llvm="yes"])

  succeeded=no
  if test -z "$ac_llvm_config_path"; then
      ac_llvm_config_path=`which llvm-config`
  fi

  if test "x$want_llvm" = "xyes"; then
      if test -e "$ac_llvm_config_path"; then
          LLVM_VERSION="$($ac_llvm_config_path --version)"
          llvm_ver_num=`echo $LLVM_VERSION | sed 's/\(@<:@0-9@:>@\{1,\}\)\.\(@<:@0-9@:>@\{1,\}\).*/\1\2/'`

          LLVM_COMPS="$1"
          if test "$llvm_ver_num" -lt "38"; then
              LLVM_COMPS="$(echo $LLVM_COMPS | sed 's|orcjit||g')"
          fi

          LLVM_CFLAGS=`$ac_llvm_config_path --cflags`
          LLVM_CXXFLAGS=`$ac_llvm_config_path --cxxflags`
          LLVM_LDFLAGS="$($ac_llvm_config_path --ldflags)"
          LLVM_SYSLIBS="$($ac_llvm_config_path --system-libs)"
          LLVM_LIBS="$($ac_llvm_config_path --libs $LLVM_COMPS) $LLVM_SYSLIBS"
          LLVM_CONFIG_BINDIR="$($ac_llvm_config_path --bindir | sed  's|\\|\\\\|g')"
          LLVM_LIBDIR="$($ac_llvm_config_path --libdir)"

          if test "$llvm_ver_num" -lt "38"; then
              AC_MSG_ERROR([LLVM version 3.8 or later required])
          fi

          if test "$llvm_ver_num" -ge "39"; then
              AC_DEFINE_UNQUOTED(LLVM_HAS_LINK_MODULES2, [1],
                                 [LLVM has LLVMLinkModules2])
              AC_DEFINE_UNQUOTED(LLVM_HAS_ORC, [1],
                                 [LLVM has ORC JIT engine])
              AC_DEFINE_UNQUOTED(LLVM_HAS_CREATE_TARGET_DATA_LAYOUT, [1],
                                 [LLVM has LLVMCreateTargetDataLayout])
          fi

          if test "$llvm_ver_num" -ge "40"; then
              AC_DEFINE_UNQUOTED(LLVM_NEW_ATTRIBUTE_API, [1],
                                 [LLVM has new attribute API])
          fi

          if test "$llvm_ver_num" -ge "50"; then
              AC_DEFINE_UNQUOTED(LLVM_NEW_ORC_API, [1],
                                 [LLVM has new ORC API])
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

          shlib="-Wl,-rpath $LLVM_LIBDIR -lLLVM-$LLVM_VERSION"

          LIBS="$LIBS_SAVED $shlib"
          export LIBS

          AC_CACHE_CHECK([for LLVM shared library],
                         ax_cv_llvm_shared,
                         [AC_LANG_PUSH([C++])
                          AC_RUN_IFELSE(
                              [AC_LANG_PROGRAM(
                                      [[@%:@include <llvm-c/Core.h>]],
                                      [[LLVMModuleCreateWithName("test"); return 0;]])],
                              ax_cv_llvm_shared=yes,
                              ax_cv_llvm_shared=no)
                          AC_LANG_POP([C++])])

          if test "x$ax_cv_llvm_shared" = "xyes"; then
              LLVM_LIBS="$shlib"
              succeeded=yes
          fi

          CFLAGS="$CFLAGS_SAVED"
          CXXFLAGS="$CXXFLAGS_SAVED"
          LDFLAGS="$LDFLAGS_SAVED"
          LIBS="$LIBS_SAVED"
      else
          succeeded=no
      fi
  fi

  if test "$succeeded" != "yes" ; then
      AC_MSG_ERROR([[We could not detect the llvm libraries make sure that llvm-config is on your path or specified by --with-llvm.]])
  else
      AC_SUBST(LLVM_CFLAGS)
      AC_SUBST(LLVM_LDFLAGS)
      AC_SUBST(LLVM_LIBS)
      AC_DEFINE(HAVE_LLVM,,[Defined if LLVM is available])
      AC_DEFINE_UNQUOTED(LLVM_VERSION,["$LLVM_VERSION"],[Version of LLVM installed])
      AC_DEFINE_UNQUOTED(LLVM_CONFIG_BINDIR,["$LLVM_CONFIG_BINDIR"],[Location of LLVM binaries])
  fi
])
