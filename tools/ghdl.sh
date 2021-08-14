#!/bin/sh -ex
mkdir -p .ghdl
ghdl -a -fsynopsys --std=08 --workdir=.ghdl ../test/regress/$1.vhd
ghdl -e -fsynopsys --std=08 --workdir=.ghdl -o .ghdl/$1 $1
ghdl -r -fsynopsys --std=08 --workdir=.ghdl $1
