entity mixed5 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of mixed5 is
    component sub is
        generic ( q : integer );
        port ( o : out std_logic_vector(6 downto 0) );
    end component;

    signal x : std_logic_vector(6 downto 0);
begin

    u: component sub
        generic map ( 42 )
        port map ( x );

    check: process is
    begin
        wait for 1 ns;
        assert x = "0101010";
        wait;
    end process;

end architecture;
