set -xe

pwd
which nvc

nvc -a --psl $TESTDIR/regress/psl11.vhd -e psl11 2>&1 | tee out.txt || true

# Adjust output to be work directory relative
sed -i -e "s/    > [^ ]*regress\/psl11/psl11/g" out.txt

diff -u $TESTDIR/regress/gold/psl11.txt out.txt
