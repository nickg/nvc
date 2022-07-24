set -xe

pwd
which nvc

if nvc --stderr=failure -a $TESTDIR/parse/names.vhd 2>/dev/null >msgs; then
  echo "expected failure"
  exit 1
fi

if ! grep "Error" msgs ; then
  echo "error messages not printed to stdout"
  exit 1
fi

