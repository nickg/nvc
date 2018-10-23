#!/bin/sh
set -e

ieee_url="https://standards.ieee.org/content/dam/ieee-standards/standards/web/download/"
vital_url="https://raw.githubusercontent.com/tgingold/ghdl/master/libraries/vital2000"

cat <<EOF
This program will download IEEE library sources from
  $ieee_url
  $vital_url
and modify them by uncommenting the definition of operator XNOR.

EOF

download_file() {
  url="$1"
  sha="$2"

  bn=$(basename "$url")

  # download
  curl -sSL -o "$bn" "$url"
  # shasum
  case `uname` in
      OpenBSD)
          check=$(sha256 -q "$bn")
          ;;
      MSYS*)
          check=$(sha256sum "$bn" | cut -d' ' -f1)
          ;;
      *)
          check=$(shasum -a 256 "$bn" | awk '{ print $1 }')
          ;;
  esac

  echo "$bn"

  if [ ! "$check" = "$sha" ]; then
    echo "Check failed! Expected $sha have $check"
    exit 1
  fi
}



cd "$(dirname "$0")/../lib/ieee"

download_file "$ieee_url/1076.2-1996_downloads.zip" "079d3fba94876683e679ef9c6f05b1bc4d7c5fb325dda9ee1af098549d9e5fc3"

download_file "$vital_url/timing_p.vhdl"            "325f57aac8326d685bceecd4f3b05bd45b944cda07eaa7d395ab589e385e14ad"
download_file "$vital_url/timing_b.vhdl"            "e0981c345ff83423f1328d9739de652fa58bbc4a30004661e43e152eeb6ebdd7"
download_file "$vital_url/prmtvs_p.vhdl"            "69f5f97a439a172b7b516314d28582aa28b8156fc9d826d096ed1e5b6aceb400"
download_file "$vital_url/prmtvs_b.vhdl"            "d06dd5f5d5c4f8b19770db8e06b6302f87cb8e30e027feef201cc780152e3b8b"
download_file "$vital_url/memory_p.vhdl"            "97c652ecc94a9f7eab13be8913776e2a98764f471e5a10dd6b9624eef8c24c11"
download_file "$vital_url/memory_b.vhdl"            "507163c4530ffe894929caf604437c9a24dd8f4b347f0c01949d230fdfcd2825"

#unzip ieee and clean directory
unzip -q 1076.2-1996_downloads.zip
rm 1076.2-1996_downloads.zip
mv 1076.2-1996_downloads/*.vhdl .
rmdir 1076.2-1996_downloads/
rm -r -f __MACOSX/

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
