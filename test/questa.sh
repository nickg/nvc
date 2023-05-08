#!/bin/sh -ex

vlib work

root=$(git rev-parse --show-toplevel)
regress=$root/test/regress

if [ -f $regress/$1.v ]; then
  vlog $regress/$1.v
fi

if [ -f $regress/$1.vhd ]; then
  vcom -2008 $regress/$1.vhd
fi

cat >/tmp/questa.do <<EOF
run
quit
EOF

vsim -batch -do /tmp/questa.do $1
