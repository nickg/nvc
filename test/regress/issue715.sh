set -xe

pwd
which nvc

cat >test.vhd <<EOF
entity test is end;
architecture test of test is
begin
assert false report "wrong arch" severity failure;
end;
EOF

nvc -a test.vhd

cat >test.vhd <<EOF
entity test is
generic (g : integer := 0);  -- Changes the checksum
end;
architecture test2 of test is
begin
assert false report "using test2" severity note;
end;
EOF

nvc -a test.vhd

ls -l work
seqno=$(cat work/_sequence)
[ "$seqno" = 1 ] || exit 1

nvc -e test -r
