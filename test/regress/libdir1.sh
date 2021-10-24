set -xe

pwd
which nvc

nvc -a $TESTDIR/regress/signal1.vhd
nvc --work=work:other -a $TESTDIR/regress/wait1.vhd

ls -l work
ls -l other

[ -f work/WORK.SIGNAL1 ] || exit 1
[ -f other/WORK.WAIT1 ] || exit 1

[ -f other/WORK.SIGNAL1 ] && exit 1
[ -f work/WORK.WAIT1 ] && exit 1

nvc --work=/foo/bar:baz && exit 1    # Invalid character in lib name

nvc --work $(pwd)/somelib -a $TESTDIR/regress/wait2.vhd

ls -l somelib

[ -f somelib/SOMELIB.WAIT2 ] || exit 1
