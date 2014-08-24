#!/bin/sh
export NVC_LIBPATH=./lib/
./bin/nvc -a ../test/regress/$1.vhd && ./bin/nvc -e $* && ./bin/nvc -r $1 --trace --stats
