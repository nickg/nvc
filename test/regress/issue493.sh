set -xe

pwd
which nvc

nvc --work=test3 -a - <<EOF
entity test3 is
  port(
    Y  : out bit);
end test3;

architecture beh of test3 is
begin
end beh;
EOF

nvc -L. --work=test2 -a - <<EOF
library test3;
use test3.all;

entity test2 is
end entity;

architecture rtl of test2 is
  component test3
    port( Y : out bit);
  end component;
begin
  i_test3 : test3 port map(Y => open);
  p_proc : process
  begin
  report "Test2";
    wait;
  end process;
end architecture;
EOF

nvc -L. -a - <<EOF
library test2;

entity test is
end entity;

architecture rtl of test is
begin
  i_test2 : entity test2.test2;
  p_proc : process
  begin
  report "Test";
    wait;
  end process;
end architecture;
EOF

nvc -L. -e test
