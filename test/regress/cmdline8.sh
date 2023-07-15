set -xe

pwd
which nvc

if nvc -a $TESTDIR/regress/signal1.vhd -e signal1 -r --stop-delta=-1 2>out; then
  echo "expected failure"
  exit 1
fi

grep -- '--stop-delta argument must be greater than zero' out || exit 1

nvc -r signal1 --stop-delta=1000000 2>out

grep -- 'the maxmimum number of supported delta cycles is 65535' out || exit 1
