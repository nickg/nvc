#!/usr/bin/env bash
#
# Called by "nvc --install vivado".
#

. "$(dirname "$BASH_SOURCE")/functions.sh"

if [ -z "$XILINX_VIVADO" ]; then
  cat >&2 <<EOF

Set the XILINX_VIVADO environment variable to the Vivado installation
directory or source the settings64.sh script.

EOF
  exit 1
elif [ ! -d "$XILINX_VIVADO/data/vhdl/src" ]; then
  cat >&2 <<EOF
$XILINX_VIVADO/data/vhdl/src not found

Ensure XILINX_VIVADO points at a valid Vivado installation directory.

EOF
  exit 1
fi

if [ -n "$MSYSTEM" ]; then
  XILINX_VIVADO="$(cygpath -u "$XILINX_VIVADO")"
fi

echo "Using Vivado installation in $XILINX_VIVADO"
echo

src="$XILINX_VIVADO/data/vhdl/src"

GLOBAL_OPTS="-M 64m"
A_OPTS="--relaxed --relative=$XILINX_VIVADO"

for STD in ${NVC_STD:-1993 2008 2019}; do
  analyse_list unisim$(std_suffix $STD) <<EOF
$src/unisims/unisim_VPKG.vhd
$src/unisims/unisim_retarget_VCOMP.vhd
EOF

  cd "$src/unimacro"
  analyse_list unimacro$(std_suffix $STD) vhdl_analyze_order

  cd "$src/unisims/primitive"
  analyse_list unisim$(std_suffix $STD) vhdl_analyze_order

  cd "$src/unisims/retarget"
  analyse_list unisim$(std_suffix $STD) vhdl_analyze_order

  cd "$src/unifast/primitive"
  analyse_list unifast$(std_suffix $STD) vhdl_analyze_order
done
