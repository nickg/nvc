#!/bin/sh -ex

vlib work
vcom -2008 ../test/regress/$1.vhd

cat >/tmp/questa.do <<EOF
run
quit
EOF

vsim -batch -do /tmp/questa.do $1
