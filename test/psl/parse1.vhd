entity parse1 is
end entity;

architecture test of parse1 is
    signal x, y, clk : bit;

    -- psl default clock is rising_edge(clk);      -- OK
begin

    -- psl assert always x -> y ;                  -- OK
    -- psl assert always x or y -> not y;          -- OK
    -- psl assert always e or y -> x;              -- Error
    -- psl assert always 5 > 2;                    -- OK
    -- psl assert not x;                           -- OK
    -- psl assert x -> (not y or x);               -- OK
    -- psl assert always (x or y) ->
    --        (y xor x) = '1';                     -- OK
    -- psl assert always (x or y) ->               -- A comment inside PSL
    --        (y xor fff) = '1';                   -- Error
    -- psl assert                                  -- Also OK
    -- psl     x -> y;                             -- OK
    -- psl assert x -> next y;                     -- OK
    -- psl assert x -> next! y;                    -- OK
    -- psl assert never x and y;                   -- OK
    -- psl assert eventually! x and y;             -- OK

end architecture;
