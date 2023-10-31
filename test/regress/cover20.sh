set -xe

pwd
which nvc

nvc -a $TESTDIR/regress/cover20.vhd -e -gG_VAL=0 --cover=statement,toggle cover20 -r
mv work/_WORK.COVER20.elab.covdb DB1.covdb

nvc -a $TESTDIR/regress/cover20.vhd -e -gG_VAL=1 --cover=statement,toggle cover20 -r
mv work/_WORK.COVER20.elab.covdb DB2.covdb

nvc -c --report html DB1.covdb DB2.covdb 2>&1 | tee out.txt

# Adjust output to be work directory relative
sed -i -e "s/[^ ]*regress\/data\//data\//g" out.txt

diff -u $TESTDIR/regress/gold/cover20.txt out.txt
