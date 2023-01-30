set -xe

pwd
which nvc

nvc -a $TESTDIR/regress/cover11.vhd -e --cover=all cover11 -r
nvc -c --item-limit=2000 \
       --exclude-file=$TESTDIR/regress/data/cover11_ef1.txt \
       --report html work/_WORK.COVER11.elab.covdb 2>&1 | tee out.txt

# Check just items until 2000 are printed
if grep -e "EXAMPLE_ARRAY(2001)" html/hier/*; then
  exit 1
fi
if ! grep -e "EXAMPLE_ARRAY(2000)" html/hier/*; then
  exit 1
fi

sed -i -e "s/[^ ]*regress\/data\//data\//g" out.txt

#diff -u $TESTDIR/regress/gold/cover11.txt out.txt
