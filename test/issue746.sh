set -e

pwd
which nvc

for i in $(seq 100); do
  rm -rf testlib
  nvc --work=testlib --init &
  nvc --work=testlib --init &
  nvc --work=testlib --init &
  nvc --work=testlib --init &
  wait
done
