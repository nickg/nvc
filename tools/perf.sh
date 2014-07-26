#!/bin/sh
export NVC_LIBPATH=./lib/std:./lib/ieee
./bin/nvc -a ../test/perf/$1.vhd && ./bin/nvc -e --native $1 && ./bin/nvc -r --stats $*
