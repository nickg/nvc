#!/bin/sh
std=${STD:-08}
export NVC_LIBPATH=./lib/
export NVC_IMP_LIB=./lib/
root=$(git rev-parse --show-toplevel)
aopts="--relaxed --psl"
if [ -e $root/test/vhpi/$1.c ]; then
  vhpi="--load lib/vhpi_test.so --vhpi-trace --vhpi-debug"
  aopts="$aopts --preserve-case"
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
if [ -f $regress/$1.tcl ]; then
  nvc --std=$std $vhpi -a $aopts $vhd $vlog -e --trace -V $* \
      --do $regress/$1.tcl
else
  nvc --std=$std $vhpi -a $aopts $vhd $vlog -e --trace -V $* \
      -r --trace --stats --exit-severity=error
fi

