entity lut4_test is
end entity;

library unisim;
use unisim.vcomponents.all;

library ieee;
use ieee.std_logic_1164.all;

architecture test of lut4_test is
    signal i : std_logic_vector(3 downto 0);
    signal o : std_logic;
begin

    uut: LUT4
        generic map (
            INIT => X"8001" )
        port map (
            o  => o,
            i0 => i(0),
            i1 => i(1),
            i2 => i(2),
            i3 => i(3) );

    process is
    begin
        i <= "0000";
        wait for 1 ns;
        assert o = '1';
        i <= "0001";
        wait for 1 ns;
        assert o = '0';
        i <= "1111";
        wait for 1 ns;
        assert o = '1';
        wait;
    end process;

end architecture;
