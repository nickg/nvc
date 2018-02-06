#!/bin/sh
mkdir -p .ghdl
ghdl -a --std=02 --workdir=.ghdl ../test/regress/$1.vhd
ghdl -e --std=02 --workdir=.ghdl -o .ghdl/$1 $1
ghdl -r --std=02 --workdir=.ghdl $1
