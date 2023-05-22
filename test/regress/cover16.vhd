library ieee;
use ieee.std_logic_1164.all;

entity cover16 is
end cover16;

architecture test of cover16 is
    signal a, b: std_logic := '0';
    signal c, d: std_logic_vector(7 downto 0) := X"00";
begin

    process begin
        c <= d and d;
        wait for 1 ns;
        c <= b and d;
        wait for 1 ns;
        c <= d and b;
        wait for 1 ns;
        a <= and d;
        wait for 1 ns;
        -- Ensure we have at least one covered expression
        a <= '1' and b;
        wait;
    end process;

end architecture;
