entity mixed4 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of mixed4 is
    component extend is
        generic ( WIDTH : integer );
        port ( i : in std_logic;
               o : out std_logic_vector(WIDTH - 1 downto 0) );
    end component;

    signal i : std_logic;
    signal o : std_logic_vector(7 downto 0);
begin

    u: component extend
        generic map (8)
        port map (i, o);

    check: process is
    begin
        i <= '1';
        wait for 1 ns;
        assert o = X"ff";
        i <= '0';
        wait for 1 ns;
        assert o = X"00";
        i <= 'U';
        wait for 1 ns;
        -- TODO: X is not propagated
        -- assert o = (7 downto 0 => 'U');
        wait;
    end process;

end architecture;
