set -xe

pwd
which nvc

nvc -a $TESTDIR/regress/cover20.vhd \
    -e -gG_VAL=0 --cover=statement,toggle --cover-file=DB1.ncdb cover20 -r

nvc -a $TESTDIR/regress/cover20.vhd \
    -e -gG_VAL=1 --cover=statement,toggle --cover-file=DB2.ncdb cover20 -r

nvc --cover-report -o html DB1.ncdb DB2.ncdb 2>&1 | grep -v '^** Debug:' | tee out.txt

# Adjust output to be work directory relative
sed -i -e "s/[^ ]*regress\/data\//data\//g" out.txt

diff -u $TESTDIR/regress/gold/cover20.txt out.txt
