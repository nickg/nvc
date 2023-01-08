set -xe

pwd
which nvc

nvc -a $TESTDIR/regress/cover2.vhd -e --cover=toggle,include-mems cover2 -r
nvc -c --report html work/_WORK.COVER2.elab.covdb 2>&1 | tee out.txt

if [ ! -f html/index.html ]; then
  echo "missing coverage report"
  exit 1
fi

diff -u $TESTDIR/regress/gold/cover2.txt out.txt
