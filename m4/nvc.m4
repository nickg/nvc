dnl Check whether TLS is emulated.
dnl Adapted from https://github.com/gcc-mirror/gcc/blob/0f1727e25f/config/tls.m4
AC_DEFUN([NVC_CHECK_EMUTLS], [
  AC_CACHE_CHECK([whether the thread-local storage support is from emutls],
                 nvc_cv_use_emutls, [
    nvc_cv_use_emutls=no
    echo '__thread int a; int b; int main() { return a = b; }' > conftest.c
    if AC_TRY_COMMAND(${CC-cc} -Werror -S -o conftest.s conftest.c 1>&AS_MESSAGE_LOG_FD); then
      if grep __emutls_get_address conftest.s > /dev/null; then
        nvc_cv_use_emutls=yes
      fi
    fi
    rm -f conftest.*
    ])
  if test "$nvc_cv_use_emutls" = "yes" ; then
    AC_DEFINE(USE_EMUTLS, 1,
              [Define to 1 if the target uses emutls for thread-local storage.])
  fi])

dnl Ensure we are not building in the MSYS environment
AC_DEFUN([NVC_CHECK_MSYS], [
  AC_MSG_CHECKING([if $CC is the MSYS compiler])
  AS_IF([$CC -dumpmachine | grep -q pc-msys],
        [AC_MSG_RESULT([yes])
         case "$MSYSTEM" in
           CLANG64)
             AC_MSG_ERROR([$CC is msys/gcc which is not supported: install mingw-w64-clang-x86_64-clang or pass CC=clang])
             ;;
           UCRT64)
             AC_MSG_ERROR([$CC is msys/gcc which is not supported: install mingw-w64-ucrt-x86_64-gcc])
             ;;
           MINGW64)
             AC_MSG_ERROR([$CC is msys/gcc which is not supported: install mingw-w64-x86_64-gcc])
             ;;
           MSYS)
             AC_MSG_ERROR([$CC is msys/gcc which is not supported])
             ;;
           *)
             AC_MSG_ERROR([$CC is msys/gcc which is not supported: install the native compiler for $MSYSTEM])
             ;;
         esac],
        [AC_MSG_RESULT([no])])])

dnl Certain options are not valid for release builds
AC_DEFUN([NVC_DEBUG_ONLY], [
  AS_IF([test x$enable_debug$enable_maintainer_mode != xyesyes],
        [AC_MSG_ERROR(m4_normalize([$1 is intended for debug/development of the simulator
                                    itself and should not be enabled in release builds]))])])
