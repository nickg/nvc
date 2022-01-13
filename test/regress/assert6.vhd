entity assert6 is
end entity;

architecture test of assert6 is
    signal x : integer;
    signal y : real;
begin

    main: process is
        type int_ptr is access integer;
        variable p : int_ptr;
    begin
        x <= 0;
        y <= 3.142;
        p := new integer'(5);
        wait for 1 ns;
        assert x = 5 severity warning;
        assert x < -1 severity warning;
        assert character'val(x) /= NUL severity warning;
        x <= 512;
        wait for 1 ns;
        assert x > 1000 severity warning;
        assert x <= 2 severity warning;
        assert x >= 2000 severity warning;
        assert y = 4.56 severity warning;

        -- This can't print a hint because "and" short-circuits
        assert (x < 0) and (x / 0 < 0) severity warning;

        assert p = null severity warning;
        wait;
    end process;

end architecture;
