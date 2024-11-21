entity clock1 is
end entity;

architecture test of clock1 is
    signal x, y, clk : bit;
begin

    -- psl assert (x -> next y)@clk'event;   -- OK
    -- psl assert always x -> y;             -- Error
    -- psl cover {x;y};                      -- Error
    -- psl assert (x -> (next y)@x'event)@clk'event;  -- Error
    -- psl assert x@(1);                     -- Error

end architecture;
