Library ieee;
use ieee.std_logic_1164.all;

entity cover is
end entity;

architecture test of cover is
    signal s : integer;
    signal l : std_logic;
    signal l_vect : std_logic_vector(7 downto 0);
begin

    p1: process is
        variable v : integer;
    begin
        v := 1;
        if s = 1 or s > 10 then
            v := 2;
        end if;
        -- coverage off
        s <= 1;
        -- coverage on
        wait;
    end process;

    p2: process
    begin
        l <= '0';
        l_vect <= (others => '0');
        wait for 1 ns;
        l <= '1';
        l_vect <= (others => '1');
        wait for 1 ns;
        l <= '0';
        l_vect <= (others => '0');
        wait;
    end process;

end architecture;
