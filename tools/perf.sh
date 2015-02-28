#!/bin/sh
export NVC_LIBPATH=./lib
./bin/nvc -a ../test/perf/$1.vhd && ./bin/nvc -e --native $1 && ./bin/nvc -r --stats $*
