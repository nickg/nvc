set -xe

pwd
which nvc

nvc --work=WORK_A -a $TESTDIR/regress/cover23.vhd \
    -e --cover=statement --cover-file=a.ncdb cover23_top_tb -r
nvc --work=WORK_B -a $TESTDIR/regress/cover23.vhd \
    -e --cover=statement --cover-file=b.ncdb cover23_unit_tb -r

nvc --cover-merge -o merged.ncdb a.ncdb b.ncdb

# Report on merged covdb without folding
nvc --cover-report -o html merged.ncdb 2>&1 | grep -v '^** Debug:' | tee out.txt

# Report with folding -> Statement coverage is higher since case choices for 3 and 4 were folded
nvc --cover-report -o html --exclude-file $TESTDIR/regress/data/cover23_ef.txt \
    merged.ncdb 2>&1 | grep -v '^** Debug:' | tee -a out.txt

# Adjust output to be work directory relative
sed -i -e "s/[^ ]*regress\/data\//data\//g" out.txt

diff -u $TESTDIR/regress/gold/cover23.txt out.txt
