#!/bin/sh
mkdir -p .ghdl
ghdl -a --workdir=.ghdl ../test/regress/$1.vhd
ghdl --elab-run --workdir=.ghdl -o .ghdl/$1 $1
