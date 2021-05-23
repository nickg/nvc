entity predef1 is
end entity;

architecture test of predef1 is
    type my_int is range 1 to 10;
    type my_enum is (X, Y, FOO, BAR);
begin

    main: process is
        variable b : boolean;
        variable i : integer;
        variable m : my_int;
        variable e : my_enum;
        variable r : real := 1.23456;
    begin
        -----------------------------------------------------------------------
        -- MINIMUM

        assert minimum(1, 2) = 1;
        m := 5;
        b := true;
        wait for 1 ns;
        assert minimum(4, m) = 4;
        assert minimum(b, false) = false;

        -----------------------------------------------------------------------
        -- MAXIMUM

        assert maximum(2, 1) = 2;
        m := 6;
        wait for 1 ns;
        assert maximum(4, m) = 6;
        assert maximum(b, false) = true;

        -----------------------------------------------------------------------
        -- TO_STRING

        assert to_string(my_int'(7)) = "7";
        assert to_string(m) = "6";
        assert to_string(foo) = "foo";
        assert to_string(e) = my_enum'image(e);

        report to_string(now);
        assert to_string(now) = "2000000 fs";
        report to_string(now, ns);
        assert to_string(now, ns) = "2 ns";
        assert to_string(value => now, unit => hr) = "0 hr";

        report to_string(r, 2);
        assert to_string(r, 2) = "1.23";
        report to_string(r, 0);
        assert to_string(r, 0)(1 to 7) = "1.23456";

        report to_string(r, "1.1f");
        assert to_string(r, "1.1f") = "1.2";

        wait;
    end process;

end architecture;
