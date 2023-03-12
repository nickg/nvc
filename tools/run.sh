#!/bin/sh
std=${STD:-93}
export NVC_LIBPATH=./lib/
export NVC_IMP_LIB=./lib/
if [ -e lib/$1.so ]; then
    vhpi="--load lib/$1.so --vhpi-trace"
fi
root=$(git rev-parse --show-toplevel)
regress=$root/test/regress
if [ -f $regress/$1.v ]; then
  vlog=$regress/$1.v
fi
if [ -z "$vlog" ] || [ -f $regress/$1.vhd ]; then
  vhd=$regress/$1.vhd
fi
PATH="./bin:../bin:$PATH"
nvc --std=$std -a --relaxed --psl $vhd $vlog -e -V $* -r --trace --stats $vhpi
