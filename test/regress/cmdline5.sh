set -xe

pwd
which nvc

nvc --std=1993 -a $TESTDIR/regress/ieee1.vhd

nvc --std=1993 --print-deps IEEE1 IEEE1-TEST \
    | grep -E ".*WORK.IEEE1-TEST: .*test/regress/ieee1.vhd .*std/STD.STANDARD .*ieee/IEEE.STD_LOGIC_1164"
