set -xe

pwd
which nvc

nvc -a --define=USER_DIRECTIVE=VALID_VALUE -DANOTHER_DIRECTIVE=ANOTHER_VALD_VALUE $TESTDIR/regress/cond4.vhd -e cond4 -r | tee out.txt

diff -u $TESTDIR/regress/gold/cond4.txt out.txt
