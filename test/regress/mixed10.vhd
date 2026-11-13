entity mixed10 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of mixed10 is
    component ports is
        port ( \next\ : in std_logic;
               \in\   : in std_logic_vector(3 downto 0);
               \out\  : out std_logic_vector(3 downto 0) );
    end component;

    signal n : std_logic := '0';
    signal i, o : std_logic_vector(3 downto 0);
begin

    u: component ports
        port map (\next\ => n, \in\ => i, \out\ => o);

    test: process is
    begin
        i <= "1010";
        wait for 1 ns;
        assert o = "0000";
        n <= '1';
        wait for 1 ns;
        assert o = "1010";
        wait;
    end process;

end architecture;
