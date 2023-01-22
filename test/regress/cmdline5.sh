set -xe

pwd
which nvc

nvc -a $TESTDIR/regress/ieee1.vhd

nvc --print-deps IEEE1 IEEE1-TEST \
    | grep -E ".*WORK.IEEE1-TEST: .*test/regress/ieee1.vhd .*std/STD.STANDARD .*ieee/IEEE.STD_LOGIC_1164"
