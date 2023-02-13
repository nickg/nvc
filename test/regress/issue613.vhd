library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test_port is
  port(
    sig : in unsigned
  );
end entity test_port;
architecture beh of test_port is
begin
    assert sig = "0100";
end architecture beh;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity issue613 is
end entity issue613;
architecture beh of issue613 is
begin
  i_ent : entity work.test_port
  port map (
    sig => to_unsigned(4, 4)
  );
end architecture beh;
