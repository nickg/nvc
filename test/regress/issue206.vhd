library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity issue206 is
end entity;

architecture a of issue206 is
begin
  main : process
    variable u : unsigned(31 downto 0) := (others => '0');
  begin
    for i in 0 to 1023 loop
      --report integer'image(i);
      u := u / 10;
    end loop;
    wait;
  end process;
end architecture;
