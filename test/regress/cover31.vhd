entity cover31 is
end entity;

architecture test of cover31 is
    signal a, b, c, d : boolean := false;
    signal x, y, z, w : boolean;
begin

    process (a, b, c, d)
    begin
        -- Only the inner "and" is covered
        -- coverage off
        x <= not
        -- coverage on
            (a and b);

        -- Fully excluded
        -- coverage off
        y <= a or b;
        -- coverage on

        -- Outer "or" excluded, inner "and" covered
        -- coverage off
        z <= c or
        -- coverage on
            (a and d);

        -- Must not be shifted by the excluded items above
        w <= c xor d;
    end process;

    process
    begin
        a <= true;
        wait for 1 ns;
        b <= true;
        d <= true;
        wait for 1 ns;
        a <= false;
        c <= true;
        wait for 1 ns;
        assert x = true;
        assert z = true;
        assert w = false;
        wait;
    end process;

end architecture;
