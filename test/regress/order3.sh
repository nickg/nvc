set -xe

pwd
which nvc

nvc -a $TESTDIR/regress/elab1.vhd $TESTDIR/regress/elab1.vhd
nvc -e elab1   # Would fail with checksum error

