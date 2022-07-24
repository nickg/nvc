set -xe

pwd
which nvc
which fstdump

nvc -a $TESTDIR/regress/wave5.vhd -e wave5 -r -w

fstdump wave5.fst > wave5.dump
diff -u $TESTDIR/regress/gold/wave5.dump wave5.dump
