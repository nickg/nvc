library ieee;
use ieee.std_logic_1164.all;

entity issue1626 is
    port (
        clk   : in  std_logic;
        count : out natural
    );
end entity;

architecture test of issue1626 is
    signal c : natural := 0;
begin
    process (clk) is
    begin
        if rising_edge(clk) then
            c <= c + 1;
        end if;
    end process;

    count <= c;
end architecture;
