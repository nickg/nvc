set -xe

pwd
which nvc

# The pragmas would cause a crash when reading a serialised design unit
nvc -a - 2>log <<EOF
entity dummy_unit is
  port(
      clk            : in  bit; -- slow clk
      o_port         : out bit_vector(1 downto 0)
  );
end entity dummy_unit;

-- coverage off

architecture rtl of dummy_unit is
begin
    o_port(0) <= clk;
    o_port(1) <= clk;
end architecture rtl;

-- coverage on
EOF

grep "ignoring pragma outside of design unit" log

nvc -e --cover dummy_unit -r
