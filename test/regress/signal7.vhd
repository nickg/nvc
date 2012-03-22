library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity signal7 is
end entity;

architecture test of signal7 is
    signal x, y : unsigned(7 downto 0);
begin

    process is
    begin
        x <= to_unsigned(5, 8);
        wait for 1 ns;
        y <= x + 1;
        wait for 1 ns;
        assert y = 6;
        wait;
    end process;

end architecture;
