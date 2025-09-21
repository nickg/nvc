set -xe

pwd
which nvc

nvc -a --psl $TESTDIR/regress/cover22.vhd \
    -e --cover=statement --cover-file=a.ncdb cover22_a -r
nvc --work=SECOND_LIB:work -a --psl $TESTDIR/regress/cover22.vhd \
    -e --cover=statement --cover-file=b.ncdb cover22_b -r

# Using -c to check deprecated command still works
nvc -c --report html a.ncdb b.ncdb 2>&1 | grep -v '^** Debug:' | tee out.txt

# Adjust output to be work directory relative
sed -i -e "s/[^ ]*regress\/data\//data\//g" out.txt

diff -u $TESTDIR/regress/gold/cover22.txt out.txt
