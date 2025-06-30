#!/bin/bash -e

root=$(git rev-parse --show-toplevel)
regress=$root/test/regress

iverilog -o /tmp/$1 $regress/$1.v
vvp /tmp/$1
