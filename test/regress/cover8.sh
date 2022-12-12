set -xe

pwd
which nvc
which fstdump

nvc -a $TESTDIR/regress/cover8.vhd -e --dump-vcode --cover=expression cover8 -r
nvc -c --report html work/_WORK.COVER8.elab.covdb 2>&1 | tee out.txt

if [ ! -f html/index.html ]; then
  echo "missing coverage report"
  exit 1
fi

diff -u $TESTDIR/regress/gold/cover8.txt out.txt
