Library ieee;
use ieee.std_logic_1164.all;

entity parse3 is
end entity;

architecture test of parse3 is
    signal a, b, c, clk : bit;
begin

    -- psl default clock is clk'event and clk = '1';

    -- psl assert never b;
    -- psl assert always (a -> next_a[3 to 5] (b));
    -- psl assert {a;b and c};
    -- psl assert a -> next [2] (b until! c);
    -- psl cover {[*]; a[*4]} report "msg";

end architecture;
