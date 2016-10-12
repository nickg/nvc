#!/bin/sh
set -e

ieee_url="http://standards.ieee.org/downloads/1076/1076.2-1996"
vital_url="http://svn.gna.org/svn/ghdl/trunk/libraries/vital2000"

cat <<EOF
This program will download IEEE library sources from
  $ieee_url
  $vital_url
and modify them by uncommenting the definition of operator XNOR.

EOF

echo $prompt

while true; do
    printf "Continue? (yes/no) "
    read yn
    case $yn in
        [Yy]* ) break ;;
        [Nn]* ) exit ;;
        * ) echo "Please answer yes or no.";;
    esac
done

download_file() {
  url="$1"
  sha="$2"

  bn=$(basename "$url")

  # download
  curl -sSL -o "$bn" "$url"
  # shasum
  check=$(shasum -a 256 "$bn" | awk '{ print $1 }')

  echo "$bn"

  if [ ! "$check" = "$sha" ]; then
    echo "Check failed!"
    exit
  fi
}

cd "$(dirname "$0")/../lib/ieee"

download_file "$ieee_url/math_complex-body.vhdl"    "9dbdfaeb790f7afea854d52b84d20406717a6cff897ccc7720f10df53581c597"
download_file "$ieee_url/math_complex.vhdl"         "118a2c237befb730c7de09c2875a9f0cc3894e53d1d4c0c1c2779b8bc6dd0a31"
download_file "$ieee_url/math_real-body.vhdl"       "384516ee1ec0488a2e765c1b3cd6ab2212361f30195a46b6e044a408db4ba99a"
download_file "$ieee_url/math_real.vhdl"            "c09a96f905221da58f24c922ebdac886f69c2e80f102b6f29bb33c6bd9bd4c76"
download_file "$ieee_url/numeric_bit-body.vhdl"     "5e0734c0b4eb70d7e6c776b6d639c526eff770eb8177f94d510678bc76c5b1c3"
download_file "$ieee_url/numeric_bit.vhdl"          "3f4dfa268a0286eb24f89f4c1fe6d6122424acde93da0369fde53cce3d7e2b94"
download_file "$ieee_url/numeric_std-body.vhdl"     "739b4a68a27629b6e82b4e83a0f2cbd6127a70fe7b0ffd9e513d148bab5064ad"
download_file "$ieee_url/numeric_std.vhdl"          "1736e832e2d1ac2e94103f8199f0a83fd6070edf9fe5c1600c16189de6dd2672"
download_file "$ieee_url/std_logic_1164-body.vhdl"  "10f391f90869db4a05c2087d434d270b24fa8427218b64b68a827e270c47acc1"
download_file "$ieee_url/std_logic_1164.vhdl"       "c6359df78524b69052c8a6f8c485effbedb152b2bb5eecd0d38043b8013658c3"
download_file "$vital_url/timing_p.vhdl"            "325f57aac8326d685bceecd4f3b05bd45b944cda07eaa7d395ab589e385e14ad"
download_file "$vital_url/timing_b.vhdl"            "e0981c345ff83423f1328d9739de652fa58bbc4a30004661e43e152eeb6ebdd7"
download_file "$vital_url/prmtvs_p.vhdl"            "69f5f97a439a172b7b516314d28582aa28b8156fc9d826d096ed1e5b6aceb400"
download_file "$vital_url/prmtvs_b.vhdl"            "d06dd5f5d5c4f8b19770db8e06b6302f87cb8e30e027feef201cc780152e3b8b"
download_file "$vital_url/memory_p.vhdl"            "97c652ecc94a9f7eab13be8913776e2a98764f471e5a10dd6b9624eef8c24c11"
download_file "$vital_url/memory_b.vhdl"            "03878edc834050f67ec5d3e0b49bef883065dc429700a174e93850274c63e458"

# Uncomment operator XNOR

std_logic_pack=
std_logic_body=

mv std_logic_1164.vhdl std_logic_1164.vhdl.bak
mv std_logic_1164-body.vhdl std_logic_1164-body.vhdl.bak

sed -e '119,120 s/^--/  /' -e '89 s/^--/  /' \
    std_logic_1164.vhdl.bak > std_logic_1164.vhdl

sed -e '367,382 s/^--/  /' -e '384,399 s/^--/  /' -e '165,168 s/^--/  /' \
    std_logic_1164-body.vhdl.bak > std_logic_1164-body.vhdl

rm *.vhdl.bak
