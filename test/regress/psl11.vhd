entity psl11 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of psl11 is
    signal clk : std_logic := '0';
    signal a   : std_logic := '0';

    default clock is rising_edge(clk);

begin

    -- psl assert never (isunknown(a));

end architecture;
