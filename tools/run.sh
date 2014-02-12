#!/bin/sh
export NVC_LIBPATH=./lib/std:./lib/ieee:./lib/nvc
./src/nvc -a ../test/regress/$1.vhd && ./src/nvc -e $* && ./src/nvc -r $1 --trace --stats
