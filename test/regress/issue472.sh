set -xe

pwd
which nvc

nvc --std=2008 -a $TESTDIR/regress/wait1.vhd

# This should set the default standard from the analysed unit
nvc -e wait1 -r

# This should error
if nvc --std=1993 -e wait1 -r; then
  echo "expected error"; exit 1
fi

# Likewise
if nvc -a $TESTDIR/regress/assert1.vhd -e wait1 -r; then
  echo "expected error"; exit 1
fi
