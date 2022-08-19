#!/usr/bin/env bash
#
# Called by "nvc --install vivado".
#

. $(dirname $BASH_SOURCE)/functions.sh

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

echo "Using Vivado installation in $XILINX_VIVADO"
echo

src=$XILINX_VIVADO/data/vhdl/src

GLOBAL_OPTS="-M 32m"
A_OPTS="--relaxed"

for STD in 1993 2008; do
  analyse_list unisim$(std_suffix $STD) <<EOF
$src/unisims/unisim_VPKG.vhd
$src/unisims/unisim_retarget_VCOMP.vhd
EOF

  WORK=unimacro$(std_suffix $STD)
  while IFS= read -r line; do
    analyse $src/unimacro/$line
  done < $src/unimacro/vhdl_analyze_order

  WORK=unisim$(std_suffix $STD)
  while IFS= read -r line; do
    analyse $src/unisims/primitive/$line
  done < $src/unisims/primitive/vhdl_analyze_order
  
  WORK=unifast$(std_suffix $STD)
  while IFS= read -r line; do
    analyse $src/unifast/primitive/$line
  done < $src/unifast/primitive/vhdl_analyze_order
done
