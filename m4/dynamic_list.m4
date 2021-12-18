#
# Check if the linker supports the -Wl,--dynamic-list= option.  Based
# code from LLVM under autoconf/m4/link_options.m4.
#

AC_DEFUN([CHECK_DYNAMIC_LIST],
[AC_CACHE_CHECK([if the linker accepts -Wl,--dynamic-list],
                [nvc_cv_linker_has_dynamic_list],
[ AC_LANG_PUSH([C])
  oldcflags="$CFLAGS"
  # The following code is from the autoconf manual,
  # "11.13: Limitations of Usual Tools".
  # Create a temporary directory $tmp in $TMPDIR (default /tmp).
  # Use mktemp if possible; otherwise fall back on mkdir,
  # with $RANDOM to make collisions less likely.
  : ${TMPDIR=/tmp}
  {
    tmp=`
      (umask 077 && mktemp -d "$TMPDIR/fooXXXXXX") 2>/dev/null
    ` &&
    test -n "$tmp" && test -d "$tmp"
  } || {
    tmp=$TMPDIR/foo$$-$RANDOM
    (umask 077 && mkdir "$tmp")
  } || exit $?
  echo "{" > "$tmp/export.map"
  echo "foo;" >> "$tmp/export.map"
  echo "};" >> "$tmp/export.map"
  CFLAGS="$CFLAGS -Wl,--dynamic-list=$tmp/export.map"
  AC_LINK_IFELSE([AC_LANG_PROGRAM([[]],[[]])],
    [nvc_cv_linker_has_dynamic_list=yes],[nvc_cv_linker_has_dynamic_list=no])
  rm "$tmp/export.map"
  rmdir "$tmp"
  CFLAGS="$oldcflags"
  AC_LANG_POP([C])
])
AS_IF([test x"AS_VAR_GET(nvc_cv_linker_has_dynamic_list)" = xyes],
  [m4_default([$1], :)],
  [m4_default([$2], :)])
])
