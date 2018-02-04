#!/bin/sh
export NVC_LIBPATH=./lib
./bin/nvc -a ../test/perf/$1.vhd -e -O3 -V $1 && ./bin/nvc -r --stats $*
