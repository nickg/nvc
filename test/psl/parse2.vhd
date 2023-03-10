entity parse2 is
end entity;

architecture test of parse2 is
    signal x, y, clk : bit;

    default clock is clk'event and clk = '1';   -- OK
begin

    assert always x -> y ;              -- OK (PSL)
    assert always x or y -> not y;      -- OK
    assert always e or y -> x;          -- Error
    assert x = '1' report "ok" severity note;  -- OK (VHDL)

end architecture;
