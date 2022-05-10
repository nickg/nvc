entity case12 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of case12 is
    signal x : std_logic_vector(1 to 3);
    signal y : natural;
begin

    p1 : process (x) is
    begin
        y <= 0;
        case? x is
            when "111"  => y <= 1;
            when "000"  => y <= 2;
            when "1--"  => y <= 3;
            when "0--"  => y <= 4;
            when "--0"  => y <= 5;
            when others => y <= 6;
        end case?;
    end process;

    p2 : process is
    begin
        x <= "000";
        wait for 1 ns;
        assert y = 2;

        x <= "XXX";
        wait for 1 ns;
        assert y = 6;

        x <= "11-";                     -- Error
        wait for 1 ns;
        assert y = 3;

        wait;
    end process;

end architecture;
