#!/bin/sh
std=${STD:-08}
export NVC_LIBPATH=./lib/
export NVC_IMP_LIB=./lib/
root=$(git rev-parse --show-toplevel)
aopts="--relaxed --psl"
name="$1"
shift
if [ -e $root/test/vhpi/$name.c ]; then
  vhpi="--load lib/vhpi_test.so --vhpi-trace --vhpi-debug"
  aopts="$aopts --preserve-case"
  export TEST_NAME=$name
fi
regress=$root/test/regress
if [ -f $regress/$name.v ]; then
  vlog=$regress/$name.v
fi
if [ -z "$vlog" ] || [ -f $regress/$name.vhd ]; then
  vhd=$regress/$name.vhd
fi
PATH="./bin:../bin:$PATH"
if [ -f $regress/$name.tcl ]; then
  nvc --std=$std $vhpi -a $aopts $vhd $vlog -e $name -V \
      --do $regress/$name.tcl $*
else
  nvc --std=$std $vhpi -a $aopts $vhd $vlog -e $name -V \
      -r --stats --exit-severity=error $*
fi

