#!/bin/bash
set -e

url="http://standards.ieee.org/downloads/1076/1076.2-1996/"

cat <<EOF
This program will download IEEE library sources from
  $url
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

files="math_complex-body.vhdl 
    math_complex.vhdl 
    math_real-body.vhdl
    math_real.vhdl 
    numeric_bit-body.vhdl
    numeric_bit.vhdl
    numeric_std-body.vhdl
    numeric_std.vhdl
    std_logic_1164-body.vhdl
    std_logic_1164.vhdl"

cd "$(dirname $0)/../lib/ieee"

for f in $files; do
    wget -nv -O $f "$url/$f"
done

# Uncomment operator XNOR
sed -e '119,120 s/^--/  /' -e '89 s/^--/  /' -i'' std_logic_1164.vhdl
sed -e '367,382 s/^--/  /' -e '384,399 s/^--/  /' \
    -e '165,168 s/^--/  /' -i'' std_logic_1164-body.vhdl
