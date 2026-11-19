entity mixed_str1 is
end entity;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture test of mixed_str1 is
    component strmod is
        generic ( MODE : string := "slow";
                  N    : integer := 3 );
        port ( o : out std_logic_vector(7 downto 0) );
    end component;

    signal o1, o2, o3 : std_logic_vector(7 downto 0);
begin

    u1: component strmod
        generic map ( MODE => "fast", N => 5 )
        port map ( o1 );

    u2: component strmod                -- Default values from component
        port map ( o2 );

    u3: component strmod
        generic map ( MODE => "fast" )
        port map ( o3 );

    test: process is
    begin
        wait for 1 ns;
        assert unsigned(o1) = 15;
        assert unsigned(o2) = 23;
        assert unsigned(o3) = 13;
        wait;
    end process;

end architecture;
