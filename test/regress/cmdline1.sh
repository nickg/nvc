set -xe

pwd
which nvc

nvc --work=foolib --init

[ -f foolib/_NVC_LIB ] || (echo "missing _NVC_LIB marker"; exit 1)
[ -f foolib/_index ] || (echo "missing _index"; exit 1)

if nvc --init foo bar; then
  echo "missing error for extra arguments"
  exit 1
fi
