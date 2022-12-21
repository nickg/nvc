set -xe

pwd
which nvc
which fstdump

nvc --std=2008 -a $TESTDIR/regress/issue586.vhd -e issue586 -r -w
fstdump issue586.fst > issue586.dump
diff -u $TESTDIR/regress/gold/issue586.dump issue586.dump
