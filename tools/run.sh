#!/bin/sh
std=${STD:-02}
export NVC_LIBPATH=./lib/
export NVC_IMP_LIB=./lib/
root=$(git rev-parse --show-toplevel)
if [ -e $root/test/vhpi/$1.c ]; then
  vhpi="--load lib/vhpi_test.so --vhpi-trace --vhpi-debug"
  export TEST_NAME=$1
fi
regress=$root/test/regress
if [ -f $regress/$1.v ]; then
  vlog=$regress/$1.v
fi
if [ -z "$vlog" ] || [ -f $regress/$1.vhd ]; then
  vhd=$regress/$1.vhd
fi
PATH="./bin:../bin:$PATH"
nvc --std=$std -a --relaxed --psl $vhd $vlog -e -V $* -r --trace --stats \
    --exit-severity=error $vhpi
