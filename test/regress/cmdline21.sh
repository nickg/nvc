set -xe

nvc -a - <<EOF
entity top1 is
end entity;
architecture a of top1 is
  signal x : integer := 99;
begin
process is
begin
  assert << signal .top2.x : integer >> = 5;
  wait;
end process;
end architecture;

entity top2 is
end entity;
architecture a of top2 is
  signal x : integer := 5;
begin
process is
begin
  assert << signal .top1.x : integer >> = 99;
  wait;
end process;
end architecture;
EOF

nvc -e top1 top2 --no-save -r
