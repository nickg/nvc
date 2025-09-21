set -xe

pwd
which nvc

nvc -a --psl $TESTDIR/regress/cover21.vhd -e --cover cover21 -r

nvc --cover-report -o html work/_WORK.COVER21.elab.covdb 2>&1 | grep -v '^** Debug:' | tee out.txt

# Adjust output to be work directory relative
sed -i -e "s/[^ ]*regress\/data\//data\//g" out.txt

diff -u $TESTDIR/regress/gold/cover21.txt out.txt
