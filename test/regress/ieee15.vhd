entity ieee15 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of ieee15 is
    signal s : std_logic_vector(15 downto 0) := X"f0f0";
begin

    tb: process is
    begin
        assert (s and X"1234") = X"1030";
        s <= s and "1101";              -- Error
        wait;
    end process;

end architecture;
