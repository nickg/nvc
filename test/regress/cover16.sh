set -xe

pwd
which nvc

nvc --std=2019 -a $TESTDIR/regress/cover16.vhd -e --cover cover16 -r
nvc --cover-report -o html work/_WORK.COVER16.elab.covdb  2>&1 | tee out.txt

# Adjust output to be work directory relative
sed -i -e "s/[^ ]*regress\/data\//data\//g" out.txt

diff -u $TESTDIR/regress/gold/cover16.txt out.txt
