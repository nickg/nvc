set -xe

pwd
which nvc

cat >test.vhd <<EOF
EOF

nvc -a - <<EOF
entity test is end;
architecture test of test is
begin
assert false report "wrong arch" severity failure;
end;
architecture test2 of test is
begin
assert false report "using test2" severity note;
end;
EOF

touch -t 199401181205.09 work/WORK.TEST-*

nvc -e test -r
