set -xe

pwd
which nvc

# Test reading from pipe: vests1 is larger than the 16kB initial buffer
nvc -a - < $TESTDIR/regress/vests1.vhd
