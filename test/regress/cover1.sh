set -xe

pwd
which nvc
which fstdump

nvc -a $TESTDIR/regress/cover1.vhd -e --cover=statement cover1 -r

nvc -c --report html work/_WORK.COVER1.elab.covdb 2>&1 | tee out.txt

if [ ! -f html/index.html ]; then
  echo "missing coverage report"
  exit 1
fi

diff -u $TESTDIR/regress/gold/cover1.txt out.txt
