entity sem1 is
end entity;

architecture test of sem1 is
    signal x, y, clk : bit;
    signal i : integer;
    constant c : integer := 5;

    -- psl default clock is clk'event and clk = '1';    -- OK
begin

    -- psl assert always i;                   -- Error

    -- psl assert x -> next[5] (y);           -- OK
    -- psl assert x -> next[-1] (y);          -- OK (during parse/sem)
    -- psl assert x -> next[x] (y);           -- Error
    -- psl assert x -> next[i] (y);           -- Error
    -- psl assert x -> next[c] (y);           -- OK
    -- psl assert x -> next i;                -- Error
    -- psl assert (next x) -> not x;          -- Error
    -- psl assert x until_ (next x);          -- Error
    -- psl assert (next x) until y;           -- Error
    -- psl assert always next_e [1 to 3] (y -> next x);   -- Error
    -- psl assert always x <-> y;             -- OK
    -- psl assert always (next x) <-> y;      -- Error
    -- psl assert always x <-> (next y);      -- Error
    -- psl assert eventually! (x -> next y);  -- Error
    -- psl assert never (x -> next y);        -- Error
    -- psl assume bad;                        -- Error
    -- psl restrict {x; bad};                 -- Error
    -- psl sequence seq1 is {not(x); x};      -- OK
    -- psl sequence seq2 is {not(f); x};      -- Error
    -- psl assert (x -> next y) before x;     -- Error
    -- psl assert x before (x -> next y);     -- Error
    -- psl assert (x -> next y) |=> {y};      -- Error
    -- psl assert (x -> next y) or (y -> next x);   -- Error
    -- psl assert {x[*]}[=2];                 -- Error
    -- psl assert {x[*]}[->1];                -- Error
    -- psl assert {x[*1 to clk]};             -- Error
    -- psl assert {z [[foo <= 1;]]};          -- Error
    -- psl endpoint e1 is {x; y};             -- Not supported

end architecture;
