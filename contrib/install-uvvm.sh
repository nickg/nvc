#!/usr/bin/env bash
#
# Called by "nvc --install uvvm".
#
# Arguments:
#  $1 = UVVM repository tag (optional) e.g. v2022.05.25

. $(dirname $BASH_SOURCE)/functions.sh

if [ -z "$1" ]; then
  branch="v2023.03.21"
fi

git_wrapper https://github.com/UVVM/UVVM $branch

STD=2008
A_OPTS="--relaxed"

component_list=$(tr -d '\r' <script/component_list.txt | tr '\n' ' ')
for component_name in $component_list; do
    echo
    echo "################################################################################"
    echo "compiling: $component_name"
    echo "################################################################################"
    echo
    cd $component_name/script
        readarray -t compile_order < compile_order.txt
        first_line=(${compile_order[0]})
        library_name=${first_line[2]}
        unset compile_order[0]
        lines=$( IFS=$'\n'; echo "${compile_order[*]}" )
        analyse_list $component_name <<<$lines
    cd ../..
done
