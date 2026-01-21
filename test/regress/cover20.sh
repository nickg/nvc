set -xe

pwd
which nvc

nvc -a $TESTDIR/regress/cover20.vhd \
    -e -gG_VAL=0 --cover=statement,toggle --cover-file=DB1.ncdb cover20 -r

nvc -a $TESTDIR/regress/cover20.vhd \
    -e -gG_VAL=1 --cover=statement,toggle --cover-file=DB2.ncdb cover20 -r

nvc --cover-merge -o merged.ncdb DB1.ncdb DB2.ncdb

nvc --cover-export --format=xml --relative=$TESTDIR/regress merged.ncdb -o out.xml

diff -u $TESTDIR/regress/gold/cover20.xml out.xml
