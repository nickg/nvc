#!/bin/bash -e

root=$(git rev-parse --show-toplevel)
regress=$root/test/regress

if [ -f "$regress/$1.sv" ]; then
  args=-g2005-sv
  file=$regress/$1.sv
else
  file=$regress/$1.v
fi

iverilog $args -o /tmp/$1 $file
vvp /tmp/$1
