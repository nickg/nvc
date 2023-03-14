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

    -- psl cover {a ;  b;c   };
    -- psl named_cover   : cover {c};

    -- psl assume b;
    -- psl named_assume     : assume (a    -> b);
    -- psl assume_guarantee ( b ->    c);

    -- psl restrict {c ;b ;   c;   b};
    -- psl named_restrict : restrict {c};
    -- psl restrict_guarantee   {b; b};

    -- psl fairness (b = '1');
    -- psl named_fairness : fairness (a = '1');
    -- psl strong fairness (a = '1'), (b = '1');

end architecture;
