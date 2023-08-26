set -xe

pwd
which nvc

mkdir empty
nvc --work=empty --init 2>errors

grep "directory empty already exists" errors

[ -f empty/_NVC_LIB ]

grep "^nvc 1\." empty/_NVC_LIB
