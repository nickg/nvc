set -xe

pwd
which nvc
which fstdump

nvc -a $TESTDIR/regress/issue536.vhd -e issue536 -r -w --dump-arrays

fstdump issue536.fst > issue536.dump
diff -u $TESTDIR/regress/gold/issue536.dump issue536.dump
