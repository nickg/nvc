set -xe

pwd
which nvc

nvc -a - -e issue850 <<EOF
entity issue850 is
end entity;

architecture test of issue850 is
begin
   p1: process is
   begin
     wait for 1 ns;
     assert false severity error;
     wait;
   end process;
end architecture;
EOF

nvc -r --exit-severity=failure issue850
