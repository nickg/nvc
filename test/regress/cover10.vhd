
library ieee;
use ieee.std_logic_1164.all;

entity cover10 is
end entity;

architecture test of cover10 is

    -- This signal will be for sure covered
    signal SIGNAL_WHICH_IS_COVERED   : std_logic;
    signal SIGNAL_WHICH_IS_EXCLUDED  : std_logic;
    signal SIGNAL_WHICH_IS_UNCOVERED : std_logic;

begin

    process
    begin
        wait for 1 ns;
        SIGNAL_WHICH_IS_COVERED <= '0';
        wait for 1 ns;
        SIGNAL_WHICH_IS_COVERED <= '1';
        wait for 1 ns;
        SIGNAL_WHICH_IS_COVERED <= '0';
        wait for 1 ns;
        wait;
    end process;


end architecture;
