set -xe

pwd
which nvc

nvc -a $TESTDIR/regress/cover25.vhd -e --cover cover25 -r 2>&1 | tee out.txt

cat out.txt | grep -e "Fatal: actual length 1 does not match formal length 0"
