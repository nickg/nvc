entity delay1 is
end entity;

architecture test of delay1 is
    signal x, y1 : bit;
    signal t : bit;
begin

    y1 <= transport x after 450 ps;

    t <= reject 1 ns inertial '1' after 1 ns when x = '1' else '0' after 2 ns;

    -- Test transport delay mechanism
    process is
    begin
        x <= '1';                       -- 0
        wait for 1 ns;
        assert y1 = '1';                -- 1000
        x <= '0';
        wait for 200 ps;
        assert y1 = '1';                -- 1200
        x <= '1';
        wait for 100 ps;
        assert y1 = '1';                -- 1300
        x <= '0';
        wait for 200 ps;
        assert y1 = '0';                -- 1500
        wait for 200 ps;
        assert y1 = '1';                -- 1700
        wait for 100 ps;
        assert y1 = '0';                -- 1800
        x <= transport '1' after 100 ps;
        x <= transport '0' after 100 ps;
        wait for 500 ps;
        assert x = '0';                 -- 2300
        assert y1 = '0';
        x <= transport '1' after 200 ps;
        x <= transport '0' after 100 ps;
        wait for 700 ps;                -- 3000
        assert x = '0';
        assert y1 = '0';
        wait;
    end process;


end architecture;
