entity ieee20 is
end entity;

library ieee;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;

architecture test of ieee20 is
begin

    check: process is
        variable x : signed(32 downto 0);
    begin
        assert to_integer(to_signed(-1, 4)) = -1;
        assert to_integer(to_signed(-5, 4)) = -5;
        assert to_integer(to_signed(-8, 4)) = -8;
        assert to_integer(to_signed(-2147483647 - 1, 32)) = -2147483647 - 1;

        x := (32 => '1', others => '0');
        wait for 1 ns;
        assert to_integer(x) = 0;
        wait;
    end process;

end architecture;
