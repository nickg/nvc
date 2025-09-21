set -xe

pwd
which nvc

nvc -a $TESTDIR/regress/cover5.vhd \
    -e -gG_VAL=1 --cover=all --cover-file=DB1.ncdb cover5 -r
nvc --cover-report -o html DB1.ncdb 2>&1 | grep -v '^** Debug:' | tee out.txt

nvc -a $TESTDIR/regress/cover5.vhd \
    -e -gG_VAL=2 --cover=all --cover-file=DB2.ncdb cover5 -r
nvc --cover-report -o html DB2.ncdb 2>&1 | grep -v '^** Debug:' | tee -a out.txt

nvc -a $TESTDIR/regress/cover5.vhd \
    -e -gG_VAL=3 --cover=all --cover-file=DB3.ncdb cover5 -r
nvc --cover-report -o html DB3.ncdb 2>&1 | grep -v '^** Debug:' | tee -a out.txt

nvc -a $TESTDIR/regress/cover5.vhd \
    -e -gG_VAL=4 --cover=all --cover-file=DB4.ncdb cover5 -r
nvc --cover-report -o html DB4.ncdb 2>&1 | grep -v '^** Debug:' | tee -a out.txt

nvc --cover-merge -o DB_MERGED.ncdb DB1.ncdb DB2.ncdb DB3.ncdb DB4.ncdb \
    --cover-report -o html DB_MERGED.ncdb \
    2>&1 | grep -v '^** Debug:' | tee -a out.txt

if [ ! -f html/index.html ]; then
  echo "missing coverage report"
  exit 1
fi

diff -u $TESTDIR/regress/gold/cover5.txt out.txt
