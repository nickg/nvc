set -xe

pwd
which nvc
which fstdump

nvc -a $TESTDIR/regress/wave14.vhd -e wave14_a wave14_b -r -w

fstdump wave14_a.fst > wave14.dump
diff -u $TESTDIR/regress/gold/wave14.dump wave14.dump
