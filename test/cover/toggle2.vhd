library ieee;
use ieee.std_logic_1164.all;

entity toggle2 is
end entity;

architecture test of toggle2 is
    type t_rec is record
        a : std_logic;
        b : natural;
        c : std_logic_vector(3 downto 0);
    end record;

    signal s1 : t_rec := ('0', 0, (others => '0'));
begin
    process
    begin
        wait for 1 ns;
        s1.a <= '1';
        s1.c <= "0101";
        wait for 1 ns;
        s1.c <= "0001";
        wait;
    end process;
end architecture;
