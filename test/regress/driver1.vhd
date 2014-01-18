entity driver1 is
end entity;

architecture test of driver1 is

    type u is (A, B, C);

    type uv is array (natural range <>) of u;

    function func (x : uv) return u is
    begin
        --report "func called";
        for i in x'range loop
          --  report u'image(x(i));
        end loop;
        for i in x'range loop
            if x(i) = A then
                return A;
            end if;
        end loop;
        return B;
    end function;

    subtype r is func u;

    signal s : r := B;
    signal k : r := C;
begin

    one: process is
    begin
        assert s = B;
        s <= A;
        wait for 1 ns;

        assert s = A;
        s <= B;
        wait for 1 ns;

        assert s = B;
        null;
        wait for 1 ns;

        assert s = A;

        wait;
    end process;

    two: process is
    begin
        assert s = B;
        s <= B;
        wait for 1 ns;

        assert s = A;
        null;
        wait for 1 ns;

        assert s = B;
        s <= A;
        wait for 1 ns;

        assert s = A;

        wait;
    end process;

    three: process is
    begin
        k <= C;
        wait for 1 ns;
        assert k = B;
        wait;
    end process;

end architecture;
