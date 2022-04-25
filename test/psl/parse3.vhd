entity parse3 is
end entity;

architecture test of parse3 is
    signal a, b, c, clk : bit;
begin

    -- psl default clock is clk'event and clk = '1';

    -- psl assert never b;
    -- psl assert always (a -> next_a[3 to 5] (b));
    -- psl assert always (a -> next[3] (b));
    -- psl assert always (a -> next_event(b)[4](c));
    -- psl assert {a};
    -- psl assert {a;b and c};

end architecture;
