entity delay1 is
end entity;

architecture test of delay1 is
    signal x, y1, y2 : bit;
begin

    y1 <= transport x after 450 ps;
    y2 <= reject 500 ps inertial x after 950 ps;

    process is
    begin
        -- Test transport delay mechanism
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
        wait for 700 ps;
        assert x = '0';                 -- 3000
        assert y1 = '0';

        assert now = 3000 ps;

        -- Test inertial delay mechanism
        assert y2 = '0';
        x <= '1';
        wait for 500 ps;
        x <= '0';                       -- 3500
        wait for 200 ps;
        x <= '1';                       -- 3700
        wait for 300 ps;
        assert y2 = '0';                -- 4000
        wait for 500 ps;
        assert y2 = '0';                -- 4500
        wait for 200 ps;
        assert y2 = '1';                -- 4700
        wait;
    end process;

end architecture;
