library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ieee3 is
end entity;

architecture test of ieee3 is
begin

    process is
        variable x, y, z: unsigned(7 downto 0);
    begin
        x := to_unsigned(3, 8);
        y := to_unsigned(5, 8);

        assert y > x;
        assert y >= x;
        assert x <= y;
        assert x < y;
        assert to_integer(x) = 3;
        assert (x + y) = 8;
        assert (y - x) = 2;
        assert (x * y) = 15;
        assert (y / x) = 1;
        wait;
    end process;

end architecture;
