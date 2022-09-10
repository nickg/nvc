set -xe

pwd
which nvc
which fstdump

nvc --std=2008 -a $TESTDIR/regress/wave7.vhd -e wave7 -r -w --stats 2>err

grep "Note: setup:" err

fstdump wave7.fst > wave7.dump
diff -u $TESTDIR/regress/gold/wave7.dump wave7.dump
