#!/bin/sh
std=${STD:-93}
export NVC_LIBPATH=./lib/
./bin/nvc --std=$std -a ../test/regress/$1.vhd -e $* -r --trace --stats
