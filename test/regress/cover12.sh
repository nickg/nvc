set -xe

pwd
which nvc

nvc -a $TESTDIR/regress/cover12.vhd -e --cover=toggle,exclude-unreachable cover12 -r
nvc --cover-report -o html cover12.ncdb 2>&1 | grep -v '^** Debug:' | tee out.txt

nvc -a $TESTDIR/regress/cover12.vhd -e --cover=toggle cover12 -r
nvc --cover-report -o html cover12.ncdb 2>&1 | grep -v '^** Debug:' | tee -a out.txt

diff -u $TESTDIR/regress/gold/cover12.txt out.txt
