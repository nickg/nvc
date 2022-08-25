set -xe

pwd
which nvc
which fstdump

nvc -a $TESTDIR/regress/issue524.vhd -e issue524 -r -w
fstdump issue524.fst > issue524.dump
diff -u $TESTDIR/regress/gold/issue524.dump issue524.dump
