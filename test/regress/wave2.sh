set -xe

pwd
which nvc
which fstdump

nvc -a $TESTDIR/regress/wave2.vhd -e wave2 -r -w --exclude '*foo' --include ':wave2:*'

fstdump wave2.fst > wave2.dump
diff -u $TESTDIR/regress/gold/wave2.dump wave2.dump
