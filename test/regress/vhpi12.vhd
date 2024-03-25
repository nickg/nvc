library ieee;
use ieee.std_logic_1164.all;

entity vhpi12 is
    port ( d, clk : in std_ulogic;
           q : out std_ulogic );
end entity;

architecture test of vhpi12 is
begin

    process (clk) is
    begin
        if rising_edge(clk) then
            q <= d;
        end if;
    end process;

end architecture;
