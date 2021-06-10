entity debug1 is
end entity;

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

architecture test of debug1 is
    signal fRn2 :  std_logic_vector(22 downto 0);
    signal fR :  std_logic_vector(23 downto 0);
    signal round :  std_logic;
begin

    warn: fRn2 <= fR(23 downto 1) + ((22 downto 1 => '0') & round); -- rounding sqrt never changes exponents

    control: process is
    begin
        wait for 1 ns;
        report "simulation finished";
        wait;
    end process;

end architecture;
