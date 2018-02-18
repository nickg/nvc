entity record13 is
end entity;

architecture test of record13 is

    type rec is record
        t : character;                  -- This struct must be packed
        x, y : integer;
    end record;

    type rec_array is array (positive range <>) of rec;

    function resolve(x : rec_array) return rec is
        variable r : rec := ('0', 0, 0);
    begin
        assert x'left = 1;
        assert x'right = x'length;

        for i in x'range loop
            r.x := r.x + x(i).x;
            r.y := r.y + x(i).y;
        end loop;

        return r;
    end function;

    subtype resolved_rec is resolve rec;

    signal sig : resolved_rec := ('0', 0, 0);
begin

    p1: process is
    begin
        sig.x <= 5;
        sig.y <= 5;
        wait for 1 ns;
        sig.t <= 'x';
        wait;
    end process;

end architecture;
