#!/bin/sh
std=${STD:-93}
export NVC_LIBPATH=./lib/
./bin/nvc --std=$std -a ../test/regress/$1.vhd && \
    ./bin/nvc --std=$std -e $* && \
    ./bin/nvc --std=$std -r $1 --trace --stats
