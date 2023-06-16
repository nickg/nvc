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

git_wrapper https://github.com/osvvm/OsvvmLibraries 2023.05

[ -d $NVC_INSTALL_DEST ] || mkdir -p $NVC_INSTALL_DEST

tclsh $SCRIPT_DIR/install-osvvm.tcl
