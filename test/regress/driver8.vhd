library ieee;
use ieee.std_logic_1164.all;

entity sub is
    generic ( n : natural range 0 to 1 );
    port ( x : inout std_logic );
end entity;

architecture test of sub is
begin

    p1: process is
    begin
        assert x = 'U';
        wait for 1 ns;
        case n is
            when 0 =>
                x <= 'L';
                wait for 0 ns;
                assert x'active;
                assert x'event;
                assert x = '0' report "expected '0' got " & std_logic'image(x);
                wait for 0 ns;
                assert x'active;
                assert x'event;
                wait for 0 ns;
                assert x'active;
                assert x'event;
                x <= '1';
                wait for 0 ns;
                assert x'active;
                assert x'event;
            when 1 =>
                x <= '0';
                wait for 0 ns;
                assert x = '0' report "expected '0' got " & std_logic'image(x);
                assert x'active;
                assert x'event report "no event: " & std_logic'image(x'last_value)
                    & " -> " & std_logic'image(x);
                x <= '1';
                wait for 0 ns;
                assert x'active;
                assert x'event;
                assert x = '1' report "expected '1' got " & std_logic'image(x);
                x <= 'Z';
                wait for 0 ns;
                assert x'active;
                assert x'event;
                assert x = 'L' report "expected 'L' got " & std_logic'image(x);
                wait for 0 ns;
                assert x'active;
                assert x'event;
                assert x = '1' report "expected '1' got " & std_logic'image(x);
                wait for 5 ns;
                assert x = 'X' report "expected 'X' got " & std_logic'image(x);
                x <= '1';
                wait for 0 ns;
                assert x'active;
                assert not x'event;
        end case;
        wait;
    end process;

    p2: process is
    begin
        x <= 'Z';
        for i in 1 to 4 loop
            wait on x;
        end loop;
        assert x = '1';
        wait for 1 ns;
        x <= '0';
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity driver8 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of driver8 is
    signal y : std_logic;
begin

    sub1: entity work.sub generic map ( 0 ) port map ( y );
    sub2: entity work.sub generic map ( 1 ) port map ( y );

    p1: process is
    begin
        assert y = 'U';
        wait for 1 ns;
        assert y = 'U' report "expected 'U' got " & std_logic'image(y);
        wait for 0 ns;
        assert y = '0' report "expected '0' got " & std_logic'image(y);
        wait for 0 ns;
        assert y = '1' report "expected '1' got " & std_logic'image(y);
        wait for 0 ns;
        assert y = 'L' report "expected 'L' got " & std_logic'image(y);
        wait for 1 ns;
        assert y = '1' report "expected '1' got " & std_logic'image(y);
        wait for 5 ns;
        assert y = 'X' report "expected 'X' got " & std_logic'image(y);
        wait;
    end process;

end architecture;
