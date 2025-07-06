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

    component combine is
        generic ( WIDTH : integer );
        port ( i : in std_logic_vector(WIDTH - 1 downto 0);
               o : out std_logic );
    end component;

    signal i1, o2 : std_logic;
    signal o1, i2 : std_logic_vector(7 downto 0);
begin

    u1: component extend
        generic map (8)
        port map (i1, o1);

    u2: component combine
        generic map (8)
        port map (i2, o2);

    check: process is
    begin
        i1 <= '1';
        wait for 1 ns;
        assert o1 = X"ff";
        i1 <= '0';
        wait for 1 ns;
        assert o1 = X"00";
        i1 <= 'U';
        wait for 1 ns;
        -- TODO: X is not propagated
        -- assert o = (7 downto 0 => 'U');

        i2 <= X"00";
        wait for 1 ns;
        assert o2 = '0';
        i2 <= X"10";
        wait for 1 ns;
        assert o2 = '1';
        i2 <= X"AA";
        wait for 1 ns;
        assert o2 = '1';

        wait;
    end process;

end architecture;
