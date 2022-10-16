set -xe

pwd
which nvc
which fstdump

nvc --std=2008 -a $TESTDIR/regress/wave8.vhd -e wave8 -r -w --stats 2>err

fstdump wave8.fst > wave8.dump
diff -u $TESTDIR/regress/gold/wave8.dump wave8.dump
