set -xe

pwd
which nvc
which fstdump

nvc -a $TESTDIR/regress/wave2.vhd -e wave2 -r -w --exclude '*foo' --include ':wave2:*'
ls -l
cp wave2.fst /tmp

fstdump wave2.fst > wave2.dump
diff -u wave2.dump $TESTDIR/regress/gold/wave2.dump
