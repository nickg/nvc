set -xe

pwd
which nvc
which fstdump

nvc -a $TESTDIR/regress/cover3.vhd -e --cover=branch cover3 -r

nvc -c --report html work/_WORK.COVER3.elab.covdb 2>&1 | tee out.txt

if [ ! -f html/index.html ]; then
  echo "missing coverage report"
  exit 1
fi

diff -u $TESTDIR/regress/gold/cover3.txt out.txt
