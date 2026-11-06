library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_signed.all;

entity synopsys8 is
end entity;

architecture test of synopsys8 is
begin

    process is
        variable n : std_logic_vector(0 downto 1);
        variable w : std_logic_vector(0 downto 1);
    begin
        w := n + '1';    -- Error
        wait;
    end process;

end architecture;
