context fifo_proj is
  library ieee;
  context ieee.ieee_std_context;
  use ieee.numeric_std_unsigned.all;
end context;

library ieee;
context ieee.ieee_std_context;

package fifo_types is
  alias logic is std_ulogic;
  alias logic_vec is std_ulogic_vector;
end package;

context work.fifo_proj;
use work.fifo_types.all;
use std.textio.all;

entity issue801 is
end entity;

architecture mixed of issue801 is
  signal clk : logic := '0';
  signal foo : logic_vec (7 downto 0) := (others => '0');
begin

foo <= x"00", x"ab" after 10 ns, x"bc" after 30 ns;

clock_gen: process is
begin
  clk <= '0';
  wait for 5 ns;
  clk <= '1';
  wait for 5 ns;
  if now > 50 ns then
      wait;
  end if;
end process;

log: postponed process (clk) is
  alias s is to_string [logic return string];
  alias h is to_hex_string [logic_vec return string];
  variable l: line;
begin
  swrite(l, "clk: " & s(clk) & ", foo: " & h(foo));
  writeline(output, l);
end process;

end architecture;
