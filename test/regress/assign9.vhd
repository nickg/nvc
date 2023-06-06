entity assign9 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of assign9 is
    signal s : integer := 0;
    signal t : std_logic_vector(1 to 2);
begin

    t <= "10";

    p1: process is
        variable v : integer;
        variable b1, b2 : std_logic;
    begin
        v := 1 when s = 0 else 5 when s = 2 else 6;
        assert v = 1;

        s <= 5;
        wait for 1 ns;

        with s select v :=
            10 when 0, 22 when 5, 2 when others;
        assert v = 22;

        s <= 6;
        wait for 1 ns;

        with s select (b1, b2) := std_logic_vector'("01") when 2, "10" when others;

        assert b1 = '1';
        assert b2 = '0';

        with t select? v :=
            2 when "-1",
            3 when "1-",
            4 when others;
        assert v = 3;

        with t select? s <=
            5 when "-0",
            8 when "1-",
            4 when others;

        wait for 0 ns;
        assert s = 5;

        wait;
    end process;

end architecture;
