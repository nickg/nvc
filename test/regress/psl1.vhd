entity psl1 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of psl1 is
    signal clk : std_logic := '0';
    signal x, y : std_logic := '0';

    -- psl default clock is rising_edge(clk);
begin

    -- psl a1: assert always x or y;
    -- psl a2: assert always x -> next y;

end architecture;
