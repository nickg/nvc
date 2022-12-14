#!/usr/bin/env bash
#
# Called by "nvc --install ise".
#

. $(dirname $BASH_SOURCE)/functions.sh

if [ -z "$XILINX" ]; then
  cat >&2 <<EOF

Set the XILINX environment variable to the Xilinx ISE installation
directory or source the settings64.sh script.

EOF
  exit 1
elif [ ! -d "$XILINX/vhdl/src" ]; then
  cat >&2 <<EOF
$XILINX/vhdl/src not found

Ensure XILINX points at a valid Xilinx ISE installation directory.

EOF
  exit 1
fi

if [ -n "$MSYSTEM" ]; then
  XILINX=$(cygpath -u $XILINX)
fi

echo "Using ISE installation in $XILINX"
echo

src=$XILINX/vhdl/src

GLOBAL_OPTS="-M 64m"
A_OPTS="--relaxed"

for STD in 1993 2008; do
  WORK=unisim$(std_suffix $STD)
  analyse $src/unisims/unisim_VPKG.vhd
  analyse $src/unisims/unisim_VCOMP.vhd

  WORK=unisim$(std_suffix $STD)
  while IFS= read -r line; do
    analyse $src/unisims/primitive/$line
  done < $src/unisims/primitive/vhdl_analyze_order
done
