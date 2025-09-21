set -xe

pwd
which nvc

nvc -a $TESTDIR/regress/cover19.vhd -e --cover=fsm-state,fsm-no-default-enums --cover-spec=$TESTDIR/regress/data/cover19_spec.txt cover19 -r
nvc --cover-report -o html work/_WORK.COVER19.elab.covdb 2>&1 | grep -v '^** Debug:' | tee out.txt

# Adjust output to be work directory relative
sed -i -e "s/[^ ]*regress\/data\//data\//g" out.txt

diff -u $TESTDIR/regress/gold/cover19.txt out.txt
