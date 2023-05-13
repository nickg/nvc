set -xe

pwd
which nvc

nvc --std=2019 -a $TESTDIR/regress/cover15.vhd -e --cover=statement,branch cover15 -r
nvc -c --report html work/_WORK.COVER15.elab.covdb  2>&1 | tee out.txt

# Adjust output to be work directory relative
sed -i -e "s/[^ ]*regress\/data\//data\//g" out.txt

diff -u $TESTDIR/regress/gold/cover15.txt out.txt