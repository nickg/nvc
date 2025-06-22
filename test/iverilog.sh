#!/bin/bash -e

root=$(git rev-parse --show-toplevel)
regress=$root/test/regress

iverilog $(realpath --relative-to . $regress/$1.v) -o /tmp/$1
vvp /tmp/$1
