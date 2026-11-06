library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

entity synopsys5 is
end entity;

architecture test of synopsys5 is
begin

    -- The result of STD_LOGIC_VECTOR + STD_LOGIC has the bounds of the
    -- vector operand
    process is
        variable v : std_logic_vector(0 to 7);
        variable w : std_logic_vector(2 to 9);
        variable d : std_logic_vector(7 downto 0);
        variable u : unsigned(0 to 7);
    begin
        v := X"7F";
        assert std_logic_vector'(v + '1')'left = 0;
        assert std_logic_vector'(v + '1')'right = 7;
        assert std_logic_vector'(v + '1')'ascending;
        assert "+"(v, '1')(0) = '1';
        assert "+"(v, '1')(7) = '0';
        assert std_logic_vector'('1' + v)'left = 0;
        assert std_logic_vector'('1' + v)'ascending;
        assert "+"('1', v)(0) = '1';

        w := X"80";
        assert std_logic_vector'(w + '1')'left = 2;
        assert std_logic_vector'(w + '1')'right = 9;
        assert "+"(w, '1')(2) = '1';
        assert "+"(w, '1')(9) = '1';
        assert std_logic_vector'('0' + w)'left = 2;
        assert std_logic_vector'('0' + w)'right = 9;

        d := X"7F";
        assert std_logic_vector'(d + '1')'left = 7;
        assert std_logic_vector'(d + '1')'right = 0;
        assert not std_logic_vector'(d + '1')'ascending;

        -- The STD_LOGIC_ARITH operators return (N-1 downto 0)
        u := X"7F";
        assert unsigned'(u + '1')'left = 7;
        assert unsigned'(u + '1')'right = 0;
        assert std_logic_vector'(u + '1')'left = 7;
        assert std_logic_vector'('1' + u)'right = 0;

        wait;
    end process;

end architecture;
