#!/bin/sh
export NVC_LIBPATH=./lib/std:./lib/ieee
./src/nvc -a ../test/perf/$1.vhd && ./src/nvc -e --native $1 && ./src/nvc -r --stats $*
