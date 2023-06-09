set -xe

pwd
which nvc

cat >compile_list.txt <<EOF
$TESTDIR/regress/wait1.vhd   # This is a comment

$TESTDIR/regress/signal1.vhd
# Another blank line
EOF

nvc -a -f compile_list.txt
nvc -e --jit --no-save wait1 -r
nvc -e --jit --no-save signal1 -r

rm -r work

nvc -a @compile_list.txt
nvc -e --jit --no-save wait1 -r
nvc -e --jit --no-save signal1 -r
