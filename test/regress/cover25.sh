set -xe

pwd
which nvc

nvc -a $TESTDIR/regress/cover25.vhd -e --cover cover25 -r 2>&1 | tee out.txt

# Adjust output to be work directory relative
sed -i '2d' out.txt

diff -u $TESTDIR/regress/gold/cover25.txt out.txt
