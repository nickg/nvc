set -xe

pwd
which nvc

nvc -a --psl $TESTDIR/regress/cover22.vhd -e --cover=statement cover22_a -r
nvc --work=SECOND_LIB:work -a --psl $TESTDIR/regress/cover22.vhd -e --cover=statement cover22_b -r

nvc -c --report html work/_WORK.COVER22_A.elab.covdb work/_SECOND_LIB.COVER22_B.elab.covdb 2>&1 | tee out.txt

# Adjust output to be work directory relative
sed -i -e "s/[^ ]*regress\/data\//data\//g" out.txt

diff -u $TESTDIR/regress/gold/cover22.txt out.txt
