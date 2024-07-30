library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity synopsys3 is
end entity;

architecture test of synopsys3 is
begin

    test_unsigned_plus: process is
        variable x, y : unsigned(15 downto 0);
    begin
        x := X"ffff";
        y := X"0002";
        assert x + y = 1;

        -- This should fall back to the interpreter
        assert x(0 downto 1) + y(0 downto 1) = unsigned'("");

        wait;
    end process;

end architecture;
