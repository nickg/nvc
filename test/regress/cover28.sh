set -xe

nvc -a $TESTDIR/regress/cover28.vhd -e --cover cover28 -r
nvc --cover-report --output=html cover28.ncdb 2>&1 | tee out.txt

diff -u $TESTDIR/regress/gold/cover28.txt out.txt
