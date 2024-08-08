set -xe

pwd
which nvc

nvc -a $TESTDIR/regress/cover18.vhd -e --cover=fsm-state cover18 -r
nvc --cover-report -o html work/_WORK.COVER18.elab.covdb --exclude=$TESTDIR/regress/data/cover18_ef.txt 2>&1 | tee out.txt

# Adjust output to be work directory relative
sed -i -e "s/[^ ]*regress\/data\//data\//g" out.txt

diff --color -u $TESTDIR/regress/gold/cover18.txt out.txt
