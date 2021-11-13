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
                assert x = '0' report "expected '0' got " & std_logic'image(x);
                wait for 0 ns;
                wait for 0 ns;
                x <= '1';
                wait for 0 ns;
            when 1 =>
                x <= '0';
                wait for 0 ns;
                assert x = '0' report "expected '0' got " & std_logic'image(x);
                x <= '1';
                wait for 0 ns;
                assert x = '1' report "expected '1' got " & std_logic'image(x);
                x <= 'Z';
                wait for 0 ns;
                assert x = 'L' report "expected 'L' got " & std_logic'image(x);
                wait for 0 ns;
                assert x = '1' report "expected '1' got " & std_logic'image(x);
        end case;
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity driver8 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of driver8 is
    signal x : std_logic;
begin

    sub1: entity work.sub generic map ( 0 ) port map ( x );
    sub2: entity work.sub generic map ( 1 ) port map ( x );

    p1: process is
    begin
        assert x = 'U';
        wait for 1 ns;
        assert x = 'U' report "expected 'U' got " & std_logic'image(x);
        wait for 0 ns;
        assert x = '0' report "expected '0' got " & std_logic'image(x);
        wait for 0 ns;
        assert x = '1' report "expected '1' got " & std_logic'image(x);
        wait for 0 ns;
        assert x = 'L' report "expected 'L' got " & std_logic'image(x);
        wait for 1 ns;
        assert x = '1' report "expected '1' got " & std_logic'image(x);
        wait;
    end process;

end architecture;
