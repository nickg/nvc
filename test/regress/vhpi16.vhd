library ieee;
use ieee.std_logic_1164.all;

entity vhpi16 is
end entity;

architecture test of vhpi16 is
    signal x : integer;
    signal v : std_logic_vector(3 downto 0);
begin

    x <= 1 after 1 ns, 2 after 2 ns, 3 after 3 ns;
    v <= "0001" after 1 ns, "0010" after 2 ns, "0011" after 3 ns;

end architecture;
