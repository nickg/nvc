entity conv11 is
end entity;

architecture test of conv11 is
    type t_int is record
        x, y : integer;
    end record;

    type t_real is record
        x, y : real;
    end record;

    function to_int (r : t_real) return t_int is
    begin
        report "(" & real'image(r.x) & ", " & real'image(r.y) & ")";
        return (integer(r.x), integer(r.y));
    end function;

    signal si : t_int := (0, 0);
    signal sr : t_real := (0.0, 0.0);
begin

    b: block is
        port ( i : in t_int;
               o : out t_real := (0.0, 0.0) );
        port map ( i => to_int(sr),
                   to_int(o) => si );
    begin
        check_inner: process is
        begin
            assert i = (0, 0);
            wait for 0 ns;
            assert i = (1, 2);
            o <= (5.6, 7.2);
            wait;
        end process;
    end block;

    check_outer: process is
    begin
        assert si = (0, 0);
        assert sr = (0.0, 0.0);

        sr <= (1.0, 2.0);
        wait for 0 ns;
        assert si = (0, 0);
        wait for 0 ns;
        assert si = (6, 7);

        wait;
    end process;

end architecture;
