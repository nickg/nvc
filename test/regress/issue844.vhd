entity issue844 is
end entity;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture test of issue844 is
    signal s : std_logic_vector(127 downto 0);
    signal o : std_logic;
begin

    ogen: with unsigned(s) select o <=
        '1' when to_unsigned(42, 128),
        '0' when others;

    check: process is
    begin
        assert o = 'U';
        wait for 0 ns;
        assert o = '0';
        s <= 128D"42";
        wait for 1 ns;
        assert o = '1';
        wait;
    end process;

end architecture;
