set -xe

pwd
which nvc

! nvc -a $TESTDIR/regress/order4.vhd 2>err

grep "package WORK.P2 depends on an obsolete" err
grep "architecture WORK.ORDER4-TEST depends on an obsolete" err
