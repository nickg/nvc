#!/bin/sh
mkdir -p .ghdl
ghdl -a --std=02 --workdir=.ghdl ../test/regress/$1.vhd
ghdl --elab-run --std=02 --workdir=.ghdl -o .ghdl/$1 $1
