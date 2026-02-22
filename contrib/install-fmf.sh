#!/usr/bin/env bash
#
# Called by "nvc --install fmf".
#

. "$(dirname "$BASH_SOURCE")/functions.sh"

git_wrapper https://github.com/nickg/fmf-packages 20080525

A_OPTS="--relaxed"

for STD in ${NVC_STD:-1993 2008}; do
  analyse_list fmf$(std_suffix $STD) <<EOF
conversions.vhd
ecl_package.vhd
ecl_utils.vhd
ff_package.vhd
gen_utils.vhd
memory.vhd
state_tab_package.vhd
switch_pkg.vhd
EOF
done
