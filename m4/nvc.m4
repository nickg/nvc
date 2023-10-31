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
