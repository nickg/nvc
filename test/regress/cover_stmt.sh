set -xe

pwd
which nvc
which fstdump

nvc -a $TESTDIR/regress/cover_stmt.vhd -e --cover=statement cover_stmt -r

nvc -c --report html work/_WORK.COVER_STMT.elab.covdb 2>&1 | tee out.txt

if [ ! -f html/index.html ]; then
  echo "missing coverage report"
  exit 1
fi

diff -u $TESTDIR/regress/gold/cover_stmt.txt out.txt
