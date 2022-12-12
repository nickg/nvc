set -xe

pwd
which nvc
which fstdump

nvc -a $TESTDIR/regress/cover7.vhd -e --dump-vcode --cover=expression cover7 -r
nvc -c --report html work/_WORK.COVER7.elab.covdb 2>&1 | tee out.txt

if [ ! -f html/index.html ]; then
  echo "missing coverage report"
  exit 1
fi

diff -u $TESTDIR/regress/gold/cover7.txt out.txt
