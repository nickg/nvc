#!/usr/bin/env bash
#
# Called by "nvc --install osvvm".
#

. $(dirname $BASH_SOURCE)/functions.sh

if ! command -v tclsh &>/dev/null; then
  echo "Error: missing tclsh command from TCL package"
  if [ -f /etc/debian_version ]; then
    echo " ==> try 'apt-get install tcl'"
  fi
  exit 1
fi

git_wrapper https://github.com/osvvm/OsvvmLibraries 2025.02

# Temporary workaround for VHDL-2008 compile error
(cd osvvm && patch -p1 -N) <<EOF
diff --git a/osvvm.pro b/osvvm.pro
index 442bf5f40c1d..a5e3c06838d0 100644
--- a/osvvm.pro
+++ b/osvvm.pro
@@ -93,7 +93,7 @@ analyze CoverageVendorApiPkg_\${::osvvm::FunctionalCoverageIntegratedInSimulator}

 analyze TranscriptPkg.vhd

-if {\$::osvvm::Support2019FilePath} {
+if {\$::osvvm::Support2019FilePath && \$::osvvm::VhdlVersion >= 2019} {
   analyze FileLinePathPkg.vhd
 } else {
   analyze deprecated/FileLinePathPkg_c.vhd
EOF

[ -d $NVC_INSTALL_DEST ] || mkdir -p $NVC_INSTALL_DEST

tclsh $SCRIPT_DIR/install-osvvm.tcl
