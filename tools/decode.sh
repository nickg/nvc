#!/bin/bash
sed 's/^.*\[\(.*\)\]$/\1/' | \
    addr2line -pife ./src/nvc | \
    grep -v '^??' | \
    sed 's/:/ /' | \
    sed 's|/.*/||' | \
    awk '{ printf("%-25s %-10s %d\n", $1, $3, $4); }'

