set -xe

pwd
which nvc

nvc -a $TESTDIR/regress/cover10.vhd -e --cover=all cover10 -r

# Print only covered
nvc --cover-report \
    --exclude-file $TESTDIR/regress/data/cover10_ef1.txt \
    --dont-print uncovered,excluded \
    -o html cover10.ncdb 2>&1 | tee out.txt

# Check nothing uncovered or excluded is there
if grep SIGNAL_WHICH_IS_UNCOVERED html/hier/*; then
  exit 1
fi
if grep SIGNAL_WHICH_IS_EXCLUDED html/hier/*; then
  exit 1
fi

# Print only uncovered
nvc --cover-report \
    --exclude-file $TESTDIR/regress/data/cover10_ef1.txt \
    --dont-print covered,excluded \
    -o html cover10.ncdb 2>&1 | tee -a out.txt

if grep SIGNAL_WHICH_IS_COVERED html/hier/*; then
  exit 1
fi
if grep SIGNAL_WHICH_IS_EXCLUDED html/hier/*; then
  exit 1
fi

# Print only excluded
nvc --cover-report \
    --exclude-file $TESTDIR/regress/data/cover10_ef1.txt \
    --dont-print covered,uncovered \
    -o html cover10.ncdb 2>&1 | tee -a out.txt

if grep SIGNAL_WHICH_IS_COVERED html/hier/*; then
  exit 1
fi
if grep SIGNAL_WHICH_IS_UNCOVERED html/hier/*; then
  exit 1
fi

# Replace absolute paths
sed -i -e "s/[^ ]*regress\/data\//data\//g" out.txt

diff -u $TESTDIR/regress/gold/cover10.txt out.txt
