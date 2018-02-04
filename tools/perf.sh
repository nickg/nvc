#!/bin/sh
export NVC_LIBPATH=./lib
./bin/nvc -a ../test/perf/$1.vhd -e -V $1 && ./bin/nvc -r --stats $*
