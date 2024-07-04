set -xe

pwd
which nvc

cat >sub.vhd <<EOF
entity sub is
port (s : in bit);
end entity;

architecture test of sub is
signal t : bit := '0';
begin
t <= not s after 1 ns;
end architecture;
EOF

cat >top.vhd <<EOF
entity top is
end entity;

architecture test of top is
signal s : bit := '0';
begin
s <= '1' after 1 ns;
uut: entity work.sub port map (s);
end architecture;
EOF

nvc --std=1993 -a sub.vhd top.vhd
nvc -e --cover --no-save --jit top -r

rm sub.vhd

ls -l work

nvc -c --report html ./work/_WORK.TOP.elab.covdb
