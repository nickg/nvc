#!/usr/bin/env bash
#
# Called by "nvc --install uvvm".
#

. $(dirname $BASH_SOURCE)/functions.sh

git_wrapper https://github.com/UVVM/UVVM v2022.05.25

STD=2008
A_OPTS="--relaxed"

analyse_list uvvm_util <<EOF
uvvm_util/src/types_pkg.vhd
uvvm_util/src/adaptations_pkg.vhd
uvvm_util/src/string_methods_pkg.vhd
uvvm_util/src/protected_types_pkg.vhd
uvvm_util/src/global_signals_and_shared_variables_pkg.vhd
uvvm_util/src/hierarchy_linked_list_pkg.vhd
uvvm_util/src/alert_hierarchy_pkg.vhd
uvvm_util/src/license_pkg.vhd
uvvm_util/src/methods_pkg.vhd
uvvm_util/src/bfm_common_pkg.vhd
uvvm_util/src/generic_queue_pkg.vhd
uvvm_util/src/data_queue_pkg.vhd
uvvm_util/src/data_fifo_pkg.vhd
uvvm_util/src/data_stack_pkg.vhd
uvvm_util/src/rand_pkg.vhd
uvvm_util/src/func_cov_pkg.vhd
uvvm_util/src/uvvm_util_context.vhd
EOF

analyse_list uvvm_vvc_framework <<EOF
uvvm_vvc_framework/src/ti_protected_types_pkg.vhd
uvvm_vvc_framework/src/ti_vvc_framework_support_pkg.vhd
uvvm_vvc_framework/src/ti_generic_queue_pkg.vhd
uvvm_vvc_framework/src/ti_data_queue_pkg.vhd
uvvm_vvc_framework/src/ti_data_fifo_pkg.vhd
uvvm_vvc_framework/src/ti_data_stack_pkg.vhd
uvvm_vvc_framework/src/ti_uvvm_engine.vhd
EOF

analyse_list bitvis_vip_scoreboard <<EOF
bitvis_vip_scoreboard/src/generic_sb_support_pkg.vhd
bitvis_vip_scoreboard/src/generic_sb_pkg.vhd
bitvis_vip_scoreboard/src/predefined_sb.vhd
EOF

analyse_list bitvis_vip_sbi <<EOF
bitvis_vip_sbi/src/transaction_pkg.vhd
bitvis_vip_sbi/src/sbi_bfm_pkg.vhd
bitvis_vip_sbi/src/vvc_cmd_pkg.vhd
uvvm_vvc_framework/src_target_dependent/td_target_support_pkg.vhd
uvvm_vvc_framework/src_target_dependent/td_vvc_framework_common_methods_pkg.vhd
bitvis_vip_sbi/src/vvc_methods_pkg.vhd
uvvm_vvc_framework/src_target_dependent/td_queue_pkg.vhd
uvvm_vvc_framework/src_target_dependent/td_vvc_entity_support_pkg.vhd
bitvis_vip_sbi/src/sbi_vvc.vhd
bitvis_vip_sbi/src/vvc_context.vhd
EOF

analyse_list bitvis_vip_uart <<EOF
bitvis_vip_uart/src/uart_bfm_pkg.vhd
bitvis_vip_uart/src/transaction_pkg.vhd
bitvis_vip_uart/src/vvc_cmd_pkg.vhd
bitvis_vip_uart/src/monitor_cmd_pkg.vhd
uvvm_vvc_framework/src_target_dependent/td_target_support_pkg.vhd
uvvm_vvc_framework/src_target_dependent/td_vvc_framework_common_methods_pkg.vhd
bitvis_vip_uart/src/vvc_methods_pkg.vhd
uvvm_vvc_framework/src_target_dependent/td_queue_pkg.vhd
uvvm_vvc_framework/src_target_dependent/td_vvc_entity_support_pkg.vhd
bitvis_vip_uart/src/uart_rx_vvc.vhd
bitvis_vip_uart/src/uart_tx_vvc.vhd
bitvis_vip_uart/src/uart_vvc.vhd
bitvis_vip_uart/src/uart_monitor.vhd
bitvis_vip_uart/src/vvc_context.vhd
EOF

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

analyse_list bitvis_vip_clock_generator <<EOF
bitvis_vip_clock_generator/src/vvc_cmd_pkg.vhd
uvvm_vvc_framework/src_target_dependent/td_target_support_pkg.vhd
uvvm_vvc_framework/src_target_dependent/td_vvc_framework_common_methods_pkg.vhd
bitvis_vip_clock_generator/src/vvc_methods_pkg.vhd
uvvm_vvc_framework/src_target_dependent/td_queue_pkg.vhd
uvvm_vvc_framework/src_target_dependent/td_vvc_entity_support_pkg.vhd
bitvis_vip_clock_generator/src/clock_generator_vvc.vhd
bitvis_vip_clock_generator/src/vvc_context.vhd
EOF

analyse_list bitvis_uart <<EOF
bitvis_uart/src/uart_pkg.vhd
bitvis_uart/src/uart_pif_pkg.vhd
bitvis_uart/src/uart_pif.vhd
bitvis_uart/src/uart_core.vhd
bitvis_uart/src/uart.vhd
EOF

analyse_list bitvis_irqc <<EOF
bitvis_irqc/src/irqc_pif_pkg.vhd
bitvis_irqc/src/irqc_pif.vhd
bitvis_irqc/src/irqc_core.vhd
bitvis_irqc/src/irqc.vhd
EOF
