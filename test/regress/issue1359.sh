set -xe

mkdir a b

nvc --work=a/lib -a --relative=$TESTDIR $(realpath $TESTDIR/regress/wait1.vhd)
(cd $TESTDIR && nvc --work=$OLDPWD/b/lib -a regress/wait1.vhd)

cmp a/lib/LIB.WAIT1 b/lib/LIB.WAIT1
