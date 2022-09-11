#!/bin/sh
export NVC_LIBPATH=./lib
std=${STD:-93}
./bin/nvc --std=$std -a ../test/perf/$1.vhd -e -O3 -V $1 && ./bin/nvc -r --stats --profile $*
