set -xe

pwd
which nvc

nvc -a $TESTDIR/regress/cover11.vhd -e --cover=toggle cover11 -r
nvc -c --item-limit=2000 \
       --exclude-file=$TESTDIR/regress/data/cover11_ef1.txt \
       --report html work/_WORK.COVER11.elab.covdb

# Check just items until 2000 are printed
if grep -e "EXAMPLE_ARRAY(1001)" html/hier/*; then
  exit 1
fi
if ! grep -e "EXAMPLE_ARRAY(1000)" html/hier/*; then
  exit 1
fi

