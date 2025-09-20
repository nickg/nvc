set -xe

# Test coverage database save/restore
nvc --std=1993 -a $TESTDIR/regress/cover1.vhd -e --cover=statement cover1

[ -f cover1.ncdb ]

nvc -r cover1

nvc --cover-export --format=xml --relative=$TESTDIR/regress cover1.ncdb > out.xml

diff -u $TESTDIR/regress/gold/cover1.xml out.xml
