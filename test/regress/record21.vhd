entity record21 is
end entity;

architecture test of record21 is

    type rec is record
        t : character;
        -- Three bytes padding
        x, y : integer;
        z : character;
        -- Three bytes padding
    end record;

    type rec_array is array (positive range <>) of rec;

    function resolve(x : rec_array) return rec is
        variable r : rec := ('0', 0, 0, 'q');
    begin
        assert x'left = 1;
        assert x'right = x'length;

        for i in x'range loop
            report "x(" & integer'image(i) & ") = (" & integer'image(x(i).x)
                & ", " & integer'image(x(i).y) & ")";
            r.x := r.x + x(i).x;
            r.y := r.y + x(i).y;
        end loop;

        return r;
    end function;

    subtype resolved_rec is resolve rec;

    signal sig : resolved_rec := ('0', 0, 0, '0');
begin

    p1: process is
    begin
        sig <= ('a', 1, 2, 'x');
        wait for 1 ns;
        sig.x <= 5;
        wait;
    end process;

    p2: process is
    begin
        sig <= ('b', 4, 5, 'y');
        wait for 1 ns;
        assert sig = ('0', 5, 7, 'q');
        wait for 1 ns;
        assert sig = ('0', 9, 7, 'q');
        wait;
    end process;

end architecture;
