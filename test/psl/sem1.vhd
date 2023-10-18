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

end architecture;
