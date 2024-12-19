set -xe

nvc -a $TESTDIR/regress/vlog14.v -D FIRST_MACRO -D SECOND_MACRO=\"VALUE_OF_SECOND_MACRO\" -e vlog14 -r 2>&1 | tee out.txt

diff -u $TESTDIR/regress/gold/vlog14.txt out.txt
