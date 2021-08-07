#!/bin/sh -ex
mkdir -p .ghdl
ghdl -a --std=08 --workdir=.ghdl ../test/regress/$1.vhd
ghdl -e --std=08 --workdir=.ghdl -o .ghdl/$1 $1
ghdl -r --std=08 --workdir=.ghdl $1
