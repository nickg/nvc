entity mixed_open1 is
end entity;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture test of mixed_open1 is
    component open_mod is
        generic ( N : integer := 7 );
        port ( o : out std_logic_vector(7 downto 0) );
    end component;

    signal o1, o2, o3 : std_logic_vector(7 downto 0);
begin

    u1: component open_mod
        generic map ( N => 5 )
        port map ( o1 );

    u2: component open_mod              -- Default value from component
        port map ( o2 );

    u3: component open_mod
        generic map ( N => open )       -- Default value from component
        port map ( o3 );

    test: process is
    begin
        wait for 1 ns;
        assert unsigned(o1) = 15;
        assert unsigned(o2) = 17;
        assert unsigned(o3) = 17;
        wait;
    end process;

end architecture;
