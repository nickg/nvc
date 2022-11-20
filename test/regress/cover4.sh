set -xe

pwd
which nvc
which fstdump

nvc -a $TESTDIR/regress/cover4.vhd -e --cover=branch cover4 -r

nvc -c --report html work/_WORK.COVER4.elab.covdb 2>&1 | tee out.txt

if [ ! -f html/index.html ]; then
  echo "missing coverage report"
  exit 1
fi

diff -u $TESTDIR/regress/gold/cover4.txt out.txt
