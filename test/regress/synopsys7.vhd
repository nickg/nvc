library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_signed.all;

entity synopsys7 is
end entity;

architecture test of synopsys7 is
begin

    process is
        variable x, z : signed(7 downto 0);
        variable a : signed(0 to 7);
        variable v, w : std_logic_vector(7 downto 0);
    begin
        x := conv_signed(-1, 8);
        z := x + '1';
        assert conv_integer(z) = 0;
        z := '1' + x;
        assert conv_integer(z) = 0;
        z := x + '0';
        assert conv_integer(z) = -1;

        x := conv_signed(127, 8);
        z := x + '1';
        assert conv_integer(z) = -128;
        w := x + '1';
        assert conv_integer(w) = -128;
        w := '1' + x;
        assert conv_integer(w) = -128;

        a := conv_signed(-2, 8);
        z := a + '1';
        assert conv_integer(z) = -1;
        z := '1' + a;
        assert conv_integer(z) = -1;

        v := X"FF";
        w := v + '1';
        assert conv_integer(w) = 0;
        w := '1' + v;
        assert conv_integer(w) = 0;
        v := X"7F";
        w := v + '1';
        assert conv_integer(w) = -128;

        w := v + 'X';
        assert is_x(w);
        w := 'U' + v;
        assert is_x(w);

        wait;
    end process;

    -- STD_LOGIC_SIGNED results have the bounds of the vector operand
    bounds: process is
        variable v : std_logic_vector(0 to 7);
        variable w : std_logic_vector(2 to 9);
    begin
        v := X"7F";
        assert std_logic_vector'(v + '1')'left = 0;
        assert std_logic_vector'(v + '1')'right = 7;
        assert "+"(v, '1')(0) = '1';
        assert "+"(v, '1')(7) = '0';
        assert std_logic_vector'('1' + v)'left = 0;
        assert "+"('1', v)(0) = '1';

        w := X"FF";
        assert std_logic_vector'(w + '1')'left = 2;
        assert std_logic_vector'(w + '1')'right = 9;
        assert "+"(w, '1')(2) = '0';
        assert std_logic_vector'('1' + w)'right = 9;
        wait;
    end process;

end architecture;
