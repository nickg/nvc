set -xe

pwd
which nvc
which fstdump

nvc --std=2008 -a $TESTDIR/regress/wave6.vhd -e wave6 -r -w

fstdump wave6.fst > wave6.dump
diff -u $TESTDIR/regress/gold/wave6.dump wave6.dump
