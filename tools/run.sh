#!/bin/sh
std=${STD:-93}
export NVC_LIBPATH=./lib/
if [ -e lib/$1.so ]; then
    vhpi="--load lib/$1.so --vhpi-trace"
fi
root=$(git rev-parse --show-toplevel)
PATH="./bin:../bin:$PATH"
nvc --std=$std -a $root/test/regress/$1.vhd -e $* -r --trace --stats $vhpi
