set -xe

pwd
which nvc

nvc -a $TESTDIR/regress/cover26.vhd -e --cover cover26 -r

# Report per-file
# This used to crash before
nvc --cover-report -o html cover26.ncdb 2>&1 | tee out.txt

# Adjust output to be work directory relative
sed -i -e "s/[^ ]*regress\/data\//data\//g" out.txt

diff -u $TESTDIR/regress/gold/cover26.txt out.txt
