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

git_wrapper https://github.com/osvvm/OsvvmLibraries 2024.07

# Temporary workaround for analysis warnings
(cd Scripts && patch -p1 -N) <<EOF
diff --git a/VendorScripts_NVC.tcl b/VendorScripts_NVC.tcl
index c9aee5443c10..31a8d4e259b6 100644
--- a/VendorScripts_NVC.tcl
+++ b/VendorScripts_NVC.tcl
@@ -175,7 +175,7 @@ proc vendor_analyze_vhdl {LibraryName FileName args} {
   variable VhdlLibraryFullPath
   variable NVC_WORKING_LIBRARY_PATH
 
-  set  GlobalOptions [concat --std=\${VhdlShortVersion} -H 128m --work=\${LibraryName}:\${NVC_WORKING_LIBRARY_PATH}.\${VhdlShortVersion} {*}\${VHDL_RESOURCE_LIBRARY_PATHS}]
+  set  GlobalOptions [concat --std=\${VhdlShortVersion} -H 128m --stderr=error --work=\${LibraryName}:\${NVC_WORKING_LIBRARY_PATH}.\${VhdlShortVersion} {*}\${VHDL_RESOURCE_LIBRARY_PATHS}]
   set  AnalyzeOptions [concat {*}\${args} \${FileName}]
   puts "nvc \${GlobalOptions} -a \$AnalyzeOptions"
   if {[catch {exec \$nvc {*}\${GlobalOptions} -a {*}\$AnalyzeOptions} AnalyzeErrorMessage]} {
EOF

[ -d $NVC_INSTALL_DEST ] || mkdir -p $NVC_INSTALL_DEST

tclsh $SCRIPT_DIR/install-osvvm.tcl
