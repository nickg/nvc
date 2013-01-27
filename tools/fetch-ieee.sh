#!/bin/bash
set -e

ieee_url="http://standards.ieee.org/downloads/1076/1076.2-1996"
vital_url="http://tams-www.informatik.uni-hamburg.de/vhdl/packages/P1076.4"

cat <<EOF
This program will download IEEE library sources from
  $ieee_url
  $vital_url
and modify them by uncommenting the definition of operator XNOR.
EOF

echo $prompt

while true; do
    read -p "Continue? (yes/no) " yn
    case $yn in
        [Yy]* ) break ;;
        [Nn]* ) exit ;;
        * ) echo "Please answer yes or no.";;
    esac
done

files="$ieee_url/math_complex-body.vhdl
    $ieee_url/math_complex.vhdl
    $ieee_url/math_real-body.vhdl
    $ieee_url/math_real.vhdl
    $ieee_url/numeric_bit-body.vhdl
    $ieee_url/numeric_bit.vhdl
    $ieee_url/numeric_std-body.vhdl
    $ieee_url/numeric_std.vhdl
    $ieee_url/std_logic_1164-body.vhdl
    $ieee_url/std_logic_1164.vhdl
    $vital_url/timing_p.vhdl
    $vital_url/timing_b.vhdl
    $vital_url/prmtvs_p.vhdl
    $vital_url/prmtvs_b.vhdl
    $vital_url/memory_p.vhdl
    $vital_url/memory_b.vhdl"

cd "$(dirname $0)/../lib/ieee"

for f in $files; do
    wget -nv -O $(basename $f) $f
done

# Uncomment operator XNOR
sed -e '119,120 s/^--/  /' -e '89 s/^--/  /' -i'' std_logic_1164.vhdl
sed -e '367,382 s/^--/  /' -e '384,399 s/^--/  /' \
    -e '165,168 s/^--/  /' -i'' std_logic_1164-body.vhdl
