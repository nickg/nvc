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
    -- psl assert ((x = '1') and (xxxx = '1'));    -- Error (issue #911)

    -- psl assert always (x -> next x) abort y;    -- OK
    -- psl assert always (x -> (y or next x));     -- OK
    -- psl assert (x or y) or (next x);            -- OK

    -- psl
    --   assert always xxx;                        -- Error (issue #1135)
    -- psl1111 assert assert assert;               -- Not PSL

    -- psl assert always x = prev(x);              -- OK
    -- psl assert always prev(y) = '1';            -- OK
    -- psl assert x union y;                       -- OK
    -- psl assert not (x union y);                 -- OK
    -- psl assert not ('1' union '0');             -- OK
    -- psl assert not ('1' union '1' union '0');   -- OK
end architecture;
