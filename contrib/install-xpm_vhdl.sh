#!/usr/bin/env bash
#
# Called by "nvc --install xpm-vhdl".
#

. $(dirname $BASH_SOURCE)/functions.sh

git_wrapper https://github.com/fransschreuder/xpm_vhdl.git master

for STD in 2008; do
  analyse_list xpm$(std_suffix $STD) <<EOF
src/xpm/xpm_VCOMP.vhd
src/xpm/xpm_cdc/hdl/xpm_cdc_single.vhd
src/xpm/xpm_cdc/hdl/xpm_cdc_array_single.vhd
src/xpm/xpm_cdc/hdl/xpm_cdc_async_rst.vhd
src/xpm/xpm_cdc/hdl/xpm_cdc_gray.vhd
src/xpm/xpm_cdc/hdl/xpm_cdc_handshake.vhd
src/xpm/xpm_cdc/hdl/xpm_cdc_low_latency_handshake.vhd
src/xpm/xpm_cdc/hdl/xpm_cdc_pulse.vhd
src/xpm/xpm_cdc/hdl/xpm_cdc_sync_rst.vhd
src/xpm/xpm_memory/hdl/xpm_memory_base.vhd
src/xpm/xpm_memory/hdl/xpm_memory_dpdistram.vhd
src/xpm/xpm_memory/hdl/xpm_memory_dprom.vhd
src/xpm/xpm_memory/hdl/xpm_memory_sdpram.vhd
src/xpm/xpm_memory/hdl/xpm_memory_spram.vhd
src/xpm/xpm_memory/hdl/xpm_memory_sprom.vhd
src/xpm/xpm_memory/hdl/xpm_memory_tdpram.vhd
src/xpm/xpm_fifo/hdl/xpm_fifo_rst.vhd
src/xpm/xpm_fifo/hdl/xpm_fifo_reg_bit.vhd
src/xpm/xpm_fifo/hdl/xpm_counter_updn.vhd
src/xpm/xpm_fifo/hdl/xpm_fifo_reg_vec.vhd
src/xpm/xpm_fifo/hdl/xpm_reg_pipe_bit.vhd
src/xpm/xpm_fifo/hdl/xpm_fifo_base.vhd
src/xpm/xpm_fifo/hdl/xpm_fifo_async.vhd
src/xpm/xpm_fifo/hdl/xpm_fifo_axi_reg_slice.vhd
src/xpm/xpm_fifo/hdl/xpm_fifo_axif.vhd
src/xpm/xpm_fifo/hdl/xpm_fifo_axil.vhd
src/xpm/xpm_fifo/hdl/xpm_fifo_axis.vhd
src/xpm/xpm_fifo/hdl/xpm_fifo_sync.vhd
EOF
done
