#!/bin/bash -e

vlib work

root=$(git rev-parse --show-toplevel)
regress=$root/test/regress
plusopt=+acc
vsimopt="-t fs"
cover=false

if $cover; then
  plusopt+=" +cover"
  vsimopt+=" -coverage"
fi

if [ -f $regress/$1.v ]; then
  vlog $plusopt $regress/$1.v
fi

if [ -f $regress/$1.sv ]; then
  vlog $plusopt $regress/$1.sv
fi

if [ -f $regress/$1.vhd ]; then
  vcom $plusopt -2008 $regress/$1.vhd
fi

rm -f /tmp/questa.do

if $cover; then
  echo "coverage save -onexit $1.ucdb" >>/tmp/questa.do
fi

cat >>/tmp/questa.do <<EOF
run -all
quit
EOF

vsim $vsimopt -batch -do /tmp/questa.do $1
