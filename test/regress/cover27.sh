set -xe

nvc -a $TESTDIR/regress/cover27.vhd -e -gG_PAR=0 --cover=statement --cover-file=cover27_a.ncdb cover27 -r
nvc --cover-export --format=xml -o cover27_a.xml cover27_a.ncdb
sed -i -e "s/[^ ]*regress//g" cover27_a.xml

nvc -a $TESTDIR/regress/cover27.vhd -e -gG_PAR=1 --cover=statement --cover-file=cover27_b.ncdb cover27 -r
nvc --cover-export --format=xml -o cover27_b.xml cover27_b.ncdb
sed -i -e "s/[^ ]*regress//g" cover27_b.xml

nvc -a $TESTDIR/regress/cover27.vhd -e -gG_PAR=2 --cover=statement --cover-file=cover27_c.ncdb cover27 -r
nvc --cover-export --format=xml -o cover27_c.xml cover27_c.ncdb
sed -i -e "s/[^ ]*regress//g" cover27_c.xml

nvc -a $TESTDIR/regress/cover27.vhd -e -gG_PAR=3 --cover=statement --cover-file=cover27_d.ncdb cover27 -r
nvc --cover-export --format=xml -o cover27_d.xml cover27_d.ncdb
sed -i -e "s/[^ ]*regress//g" cover27_d.xml

nvc --cover-merge cover27*.ncdb --output=merged.covdb
nvc --cover-report --output=html merged.covdb 2>&1 | tee out.txt

diff -u $TESTDIR/regress/gold/cover27.txt out.txt
diff -u $TESTDIR/regress/gold/cover27_a.xml cover27_a.xml
diff -u $TESTDIR/regress/gold/cover27_b.xml cover27_b.xml
diff -u $TESTDIR/regress/gold/cover27_c.xml cover27_c.xml
diff -u $TESTDIR/regress/gold/cover27_d.xml cover27_d.xml
