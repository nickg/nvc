set -xe

pwd
which nvc

nvc --std=2000 -a $TESTDIR/regress/link3.vhd

# Elaborate as separate step
nvc --std=2000 -e link3 -r
