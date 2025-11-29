set -xe

pwd
which nvc

nvc --work=mylib -a - <<EOF
entity sub is
  port (o : out integer);
end entity;
architecture a of sub is
begin
  o <= 42;
end architecture;
EOF

nvc -L. -a - <<EOF
library mylib;
entity top is end entity;
architecture test of top is
  component sub is
    port (o : out integer);
  end component;
  signal s : integer;
begin
  u: component sub port map (s);
  process is
  begin
    wait for 0 ns;
    assert s = 42;
    wait;
  end process;
end architecture;
EOF

nvc -L. -e top --no-save -r --trace
