set -xe

nvc -a $TESTDIR/regress/cover27.vhd -e -gG_PAR=0 --cover=statement --cover-file=cover27_a.ncdb cover27 -r
nvc --cover-export --format=xml -o cover27_a.xml cover27_a.ncdb
sed -i -e "s/[^ ]*regress//g" cover27_a.xml

nvc -a $TESTDIR/regress/cover27.vhd -e -gG_PAR=1 --cover=statement --cover-file=cover27_b.ncdb cover27 -r
nvc --cover-export --format=xml -o cover27_b.xml cover27_b.ncdb
sed -i -e "s/[^ ]*regress//g" cover27_b.xml

nvc --cover-merge cover27*.ncdb --output=merged.covdb
nvc --cover-report --output=html merged.covdb 2>&1 | grep -v '^** Debug:' | tee out.txt

diff -u $TESTDIR/regress/gold/cover27.txt out.txt
diff -u $TESTDIR/regress/gold/cover27_a.xml cover27_a.xml
diff -u $TESTDIR/regress/gold/cover27_b.xml cover27_b.xml
