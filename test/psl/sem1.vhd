entity sem1 is
end entity;

architecture test of sem1 is
    signal x, y, clk : bit;
    signal i : integer;

    -- psl default clock is rising_edge(clk);      -- OK
begin

    -- psl assert always i;                   -- Error

end architecture;
