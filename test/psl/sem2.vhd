entity sem1 is
end entity;

architecture test of sem1 is
    signal x, y, clk : bit;
    signal i : integer;
    constant c : integer := 5;

    -- psl default clock is clk'event and clk = '1';    -- OK
begin

    -- psl assert always prev(x);    -- OK
    -- psl assert rose(i);           -- Error
    -- psl assert rose(x);           -- OK
    -- psl assert ended({x;y});      -- OK

end architecture;
