library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test1 is
  port (
    inp    : in  std_logic_vector(0 to 1)
    );
end entity test1;
architecture beh of test1 is
begin

  process
  begin
    wait for 1 ps;
    assert not is_X(inp(0)) report "inp0 is X" severity error;
    assert not is_X(inp(1)) report "inp1 is X" severity error;
    assert inp(0) = '1' report "inp0 is not 1" severity error;
    assert inp(1) = '0' report "inp1 is not 0" severity error;
    wait;
  end process;

end architecture beh;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity issue595 is
end entity issue595;
architecture beh of issue595 is
  signal inp0 : std_logic;
  signal inp1 : std_logic;
begin
  i_test : entity work.test1
    port map(
      inp => (inp1, inp0)
    );
  process
  begin
    inp0 <= '0';
    inp1 <= '1';
    wait;
  end process;

end architecture beh;
