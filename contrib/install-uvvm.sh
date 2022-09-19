#!/usr/bin/env bash
#
# Called by "nvc --install uvvm".
#
# Arguments:
#  $1 = UVVM repository tag (optional) e.g. v2022.05.25

. $(dirname $BASH_SOURCE)/functions.sh

if [ -z "$1" ]; then
    branch="v2022.05.25"
fi

git_wrapper https://github.com/UVVM/UVVM $branch

STD=2008
A_OPTS="--relaxed"

# Input is not strictly valid VHDL
patch -N -p1 <<EOF
diff --git a/bitvis_vip_clock_generator/src/clock_generator_vvc.vhd b/bitvis_vip_clock_generator/src/clock_generator_vvc.vhd
index 91cbde8499ce..99e92fdf13c8 100644
--- a/bitvis_vip_clock_generator/src/clock_generator_vvc.vhd
+++ b/bitvis_vip_clock_generator/src/clock_generator_vvc.vhd
@@ -75,7 +75,7 @@ architecture behave of clock_generator_vvc is
   alias vvc_status : t_vvc_status is shared_clock_generator_vvc_status(GC_INSTANCE_IDX);
   alias transaction_info : t_transaction_info is shared_clock_generator_transaction_info(GC_INSTANCE_IDX);

-  alias clock_name      : string is vvc_config.clock_name;
+  alias clock_name     is vvc_config.clock_name;
   alias clock_period    : time   is vvc_config.clock_period;
   alias clock_high_time : time   is vvc_config.clock_high_time;

EOF

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
