set -xe

pwd
which nvc
which fstdump

nvc -a $TESTDIR/regress/cover1.vhd -e --cover cover1 -r

nvc -c --report html work/_WORK.COVER1.elab.covdb 2>&1 | tee out.txt

# TODO: perhaps should be index.html?
if [ ! -f html/coverage_report.html ]; then
  echo "missing coverage report"
  exit 1
fi

diff -u $TESTDIR/regress/gold/cover1.txt out.txt
