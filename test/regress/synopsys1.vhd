library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

entity synopsys1 is
end entity;

architecture test of synopsys1 is
begin

    process is
        variable x, y, z : std_logic_vector(7 downto 0);
    begin
        x := conv_std_logic_vector(5, 8);
        y := conv_std_logic_vector(3, 8);
        z := x + y;
        assert conv_integer(z) = 8;
        z := x - y;
        assert conv_integer(z) = 2;
        assert conv_integer(x * y) = 15;
        wait;
    end process;

end architecture;
