set -xe

pwd
which nvc

nvc --work=OTHER_LIB:other_lib -a $TESTDIR/regress/cover30.vhd

nvc --map=OTHER_LIB:other_lib \
    -a $TESTDIR/regress/cover30.vhd \
    -e --cover=all \
       --cover-spec=$TESTDIR/regress/data/cover30.spec \
       --cover-file=cover30.ncdb cover30 -r

nvc --cover-report -o html cover30.ncdb 2>&1 | grep -v '^** Debug:' | tee out.txt

diff -u $TESTDIR/regress/gold/cover30.txt out.txt
