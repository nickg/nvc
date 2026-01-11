library ieee;
use ieee.std_logic_1164.all;

entity toggle1 is
    generic (
        G_VAL : integer
    );
end entity;

architecture test of toggle1 is
    signal vect : std_logic_vector(15 downto 0) := x"AAAA";
begin
    process
    begin
        wait for 1 ns;
        for i in 1 to G_VAL loop
            vect(i * G_VAL - 1) <= not vect(i * G_VAL - 1);
        end loop;
        wait for 1 ns;
        wait;
    end process;
end architecture;
