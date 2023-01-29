#!/bin/bash

if [ -z "$NVC_INSTALL_DEST" ]; then
  if command -v cygpath &>/dev/null; then
    export NVC_INSTALL_DEST="$(cygpath -m $HOME)/.nvc/lib"
  else
    export NVC_INSTALL_DEST=$HOME/.nvc/lib
  fi
fi

export SCRIPT_DIR=$(dirname $BASH_SOURCE)

_safe () {
  echo $*
  $*
  [ $? = 0 ] || exit 1
}

_nvc () {
  local _work=${WORK:-work}
  local _dest=$NVC_INSTALL_DEST
  local _opts="--std=${STD:-1993} --work=$_dest/$_work -L$_dest $GLOBAL_OPTS"
  [ -d $_dest ] || _safe mkdir -p $_dest
  _safe ${NVC:-nvc} $_opts  $*
}

analyse () {
  local _files=$*
  _nvc -a $A_OPTS $_files
}

analyse_list () {
  local _work=$1
  while read _src; do
    WORK=$_work analyse $_src
  done
}

git_wrapper () {
  local _repo=$1
  local _base=$(basename $_repo)
  local _tag=$2
  local _cache=${CACHE:-$HOME/.cache/nvc}/$_base-$_tag
  if [ ! -d $_cache/.git ]; then
    _safe mkdir -p $_cache
    _safe git clone --recursive $_repo -b $_tag $_cache --depth=1
  fi
  _safe cd $_cache
}

std_suffix () {
  case "$1" in
    1993) ;;
    2008) echo ".08" ;;
    2019) echo ".19" ;;
  esac
}
