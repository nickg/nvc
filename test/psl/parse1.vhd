entity parse1 is
end entity;

architecture test of parse1 is
    signal x, y, clk : bit;

    -- psl default clock is clk'event and clk = '1';   -- OK
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
    --        (y xor fff) = x;                     -- Error
    -- psl assert                                  -- Also OK
    -- psl     x -> y;                             -- OK
    -- psl assert x -> next y;                     -- OK
    -- psl assert x -> next! y;                    -- OK
    -- psl assert never x and y;                   -- OK
    -- psl assert eventually! x and y;             -- OK

    -- psl foo: assert x;                          -- OK
    -- psl foo: assert not x;                      -- Error

    -- psl assert x -> (x until y);                -- OK
    -- psl assert x -> (x until_ y);               -- OK
    -- psl assert x -> (x until! y);               -- OK
    -- psl assert x -> (x until_! y);              -- OK
end architecture;
