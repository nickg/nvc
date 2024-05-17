entity issue890 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of issue890 is
    constant A : std_logic_vector(1 to 3) := "101";
    constant B : std_logic_vector(1 to 3) := "-1-";
    constant C : std_logic_vector(1 to 3) := "000";

    signal i : std_logic_vector(1 to 3);
    signal o : natural;
begin

    gen: process (all) is
    begin
        case? i is
            when A => o <= 5;
            when B | C => o <= 42;
            when others => o <= 0;
        end case?;
    end process;

    check: process is
    begin
        i <= "000";
        wait for 1 ns;
        assert o = 42;
        i <= "111";
        wait for 1 ns;
        assert o = 42;
        i <= "101";
        wait for 1 ns;
        assert o = 5;
        i <= "001";
        wait for 1 ns;
        assert o = 0;
        wait;
    end process;

end architecture;
