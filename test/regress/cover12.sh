set -xe

pwd
which nvc

nvc -a $TESTDIR/regress/cover12.vhd -e --cover=toggle,exclude-unreachable cover12 -r
nvc -c --report html work/_WORK.COVER12.elab.covdb 2>&1 | tee out.txt

nvc -a $TESTDIR/regress/cover12.vhd -e --cover=toggle cover12 -r
nvc -c --report html work/_WORK.COVER12.elab.covdb 2>&1 | tee -a out.txt

diff -u $TESTDIR/regress/gold/cover12.txt out.txt