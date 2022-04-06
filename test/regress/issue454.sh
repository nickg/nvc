set -xe

pwd
which nvc
which fstdump

nvc -a $TESTDIR/regress/issue454.vhd -e issue454 -r -w

fstdump issue454.fst > issue454.dump
diff -u issue454.dump $TESTDIR/regress/gold/issue454.dump
