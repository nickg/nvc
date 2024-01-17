#!/usr/bin/env bash
#
# Called by "nvc --install libero".
#

. $(dirname $BASH_SOURCE)/functions.sh

if [ -z "$LIBERO" ]; then
  cat >&2 <<EOF

Set the LIBERO environment variable to the Microchip Libero
installation directory.

EOF
  exit 1
elif [ ! -d "$LIBERO/Designer/lib" ]; then
  cat >&2 <<EOF
$LIBERO/vhdl/src not found

Ensure LIBERO points at a valid Libero installation directory.

EOF
  exit 1
fi

if [ -n "$MSYSTEM" ]; then
  LIBERO=$(cygpath -u $LIBERO)
fi

echo "Using Microchip Libero installation in $LIBERO"
echo

src=$LIBERO/Designer/lib/

GLOBAL_OPTS="-M 64m"
A_OPTS="--relaxed"

for STD in ${NVC_STD:-1993 2008}; do
  WORK=axcelerator$(std_suffix $STD)
  analyse $src/vtl/95/axcelerator.vhd
done
