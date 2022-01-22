set -xe

pwd
which nvc
which fstdump

nvc -a $TESTDIR/regress/wave3.vhd -e wave3 -r -w --stop-time=1us

fstdump wave3.fst > wave3.dump
diff -u wave3.dump $TESTDIR/regress/gold/wave3.dump
