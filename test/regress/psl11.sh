set -xe

pwd
which nvc

nvc -a --psl $TESTDIR/regress/psl11.vhd -e psl11 2&> out.txt || true

# Adjust output to be work directory relative
sed -i -e "s/[^ ]*regress\/data\//data\//g" out.txt

diff -u $TESTDIR/regress/gold/psl11.txt out.txt
