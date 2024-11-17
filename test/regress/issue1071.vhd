library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use std.env.finish;

entity issue1071 is
end issue1071;

architecture sim of issue1071 is

  subtype A is unsigned(7 downto 0);
  type B is array (natural range <>) of A;
  signal C : B(0 to 1) := (others => (others => '0'));

  type D is array (0 to 0) of B(0 to 1);
  signal E : D := (others => (others => (others => '0')));

begin

  E(0) <= C;

  process
  begin
    wait for 10 ns;

    for i in C'range loop
      C(i) <= to_unsigned(C'length - i, A'length);
    end loop;

    wait for 10 ns;

    finish;
  end process;

end architecture;
