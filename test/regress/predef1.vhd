entity predef1 is
end entity;

architecture test of predef1 is
    type my_int is range 1 to 10;
begin

    main: process is
        variable b : boolean;
        variable i : integer;
        variable m : my_int;
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

        wait;
    end process;

end architecture;
