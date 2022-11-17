set -xe

pwd
which nvc
which fstdump

nvc -a $TESTDIR/regress/cover_toggle.vhd -e --cover=toggle cover_toggle -r

nvc -c --report html work/_WORK.COVER_TOGGLE.elab.covdb 2>&1 | tee out.txt

if [ ! -f html/index.html ]; then
  echo "missing coverage report"
  exit 1
fi

diff -u $TESTDIR/regress/gold/cover_toggle.txt out.txt
