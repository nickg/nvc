set -xe

pwd
which nvc

nvc -a $TESTDIR/regress/cover24.vhd -e --cover cover24 -r

# Report per-file
nvc --cover-report --per-file -o html  work/_WORK.COVER24.elab.covdb 2>&1 | tee out.txt

# Adjust output to be work directory relative
sed -i -e "s/[^ ]*regress\/data\//data\//g" out.txt

diff -u $TESTDIR/regress/gold/cover24.txt out.txt
