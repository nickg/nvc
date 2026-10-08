entity issue1681 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of issue1681 is
    component issue1681_sub is
        port (
            a    : in std_logic_vector(7 downto 0);
            pass : out std_logic );
    end component;

    signal a    : std_logic_vector(7 downto 0) := X"2a";
    signal pass : std_logic;
begin
    u: component issue1681_sub port map (a, pass);

    process is
    begin
        wait for 1 ns;
        assert pass = '1';
        a <= X"00";
        wait for 1 ns;
        assert pass = '0';
        wait;
    end process;
end architecture;
