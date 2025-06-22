#!/bin/sh -ex
std=${STD:-08}
mkdir -p .ghdl
ghdl -a -fpsl -frelaxed -fsynopsys --std=$std --workdir=.ghdl ../test/regress/$1.vhd
ghdl -e -fpsl -frelaxed -fsynopsys --std=$std --workdir=.ghdl -o .ghdl/$1 $1
ghdl -r -fpsl -fsynopsys --std=$std --workdir=.ghdl $1
