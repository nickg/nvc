set -xe

pwd
which nvc

nvc -a $TESTDIR/regress/cover5.vhd -e -gG_VAL=1 --cover=all cover5 -r
mv work/_WORK.COVER5.elab.covdb DB1.covdb
nvc --cover-report -o html DB1.covdb 2>&1 | tee out.txt

nvc -a $TESTDIR/regress/cover5.vhd -e -gG_VAL=2 --cover=all cover5 -r
mv work/_WORK.COVER5.elab.covdb DB2.covdb
nvc --cover-report -o html DB2.covdb 2>&1 | tee -a out.txt

nvc -a $TESTDIR/regress/cover5.vhd -e -gG_VAL=3 --cover=all cover5 -r
mv work/_WORK.COVER5.elab.covdb DB3.covdb
nvc --cover-report -o html DB3.covdb 2>&1 | tee -a out.txt

nvc -a $TESTDIR/regress/cover5.vhd -e -gG_VAL=4 --cover=all cover5 -r
mv work/_WORK.COVER5.elab.covdb DB4.covdb
nvc --cover-report -o html DB4.covdb 2>&1 | tee -a out.txt

nvc --cover-merge -o DB_MERGED.covdb DB1.covdb DB2.covdb DB3.covdb DB4.covdb \
    --cover-report -o html DB_MERGED.covdb \
    2>&1 | tee -a out.txt

if [ ! -f html/index.html ]; then
  echo "missing coverage report"
  exit 1
fi

diff -u $TESTDIR/regress/gold/cover5.txt out.txt
