set -xe

nvc --std=2019 -a - <<EOF
entity test is
end entity;

library ieee;
use ieee.numeric_std.all;
use std.env.all;

architecture test of test is
begin
    process is
	variable x : unsigned(7 downto 0);
    begin
	report to_string(to_integer(x));  -- No warning
	wait for 0 ns;
	report to_string(to_integer(x));  -- No warning
	wait for 1 ns;
	report to_string(to_integer(x));  -- Warning
	assert GetVhdlAssertCount(warning) = 1;
	wait;
    end process;
end architecture;
EOF

nvc --ieee-warnings=off-at-0 -e test --no-save -r
