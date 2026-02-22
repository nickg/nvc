#!/usr/bin/env bash
#
# Called by "nvc --install ise".
#

. "$(dirname "$BASH_SOURCE")/functions.sh"

if [ -z "$ICECUBE2" ]; then
  cat >&2 <<EOF

Set the ICECUBE2 environment variable to the Lattice iCEcube2
installation directory.

EOF
  exit 1
elif [ ! -d "$ICECUBE2/vhdl" ]; then
  cat >&2 <<EOF
$ICECUBE2/vhdl/src not found

Ensure ICECUBE2 points at a valid Lattice iCEcube2 installation directory.

EOF
  exit 1
fi

if [ -n "$MSYSTEM" ]; then
  ICECUBE2="$(cygpath -u "$ICECUBE2")"
fi

echo "Using iCEcube2 installation in $ICECUBE2"
echo

src="$ICECUBE2/vhdl"

GLOBAL_OPTS="-M 64m"
A_OPTS="--relaxed"

for STD in ${NVC_STD:-1993 2008}; do
  WORK=ice$(std_suffix $STD)
  analyse "$src/vcomponent_vital.vhd"
  analyse "$src/sb_ice_syn_vital.vhd"
  analyse "$src/sb_ice_lc_vital.vhd"
done
