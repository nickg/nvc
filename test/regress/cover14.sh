set -xe

pwd
which nvc

nvc -a $TESTDIR/regress/cover14.vhd -e --cover=expression,exclude-unreachable cover14 -r
nvc --cover-report -o html work/_WORK.COVER14.elab.covdb --exclude-file $TESTDIR/regress/data/cover14_ef1.txt 2>&1 | tee out.txt

# Adjust output to be work directory relative
sed -i -e "s/[^ ]*regress\/data\//data\//g" out.txt

diff -u $TESTDIR/regress/gold/cover14.txt out.txt
