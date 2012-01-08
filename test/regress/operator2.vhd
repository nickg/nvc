entity operator2 is
end entity;

architecture test of operator2 is

    type t is (A, B);
    type tv is array (integer range <>) of t;

    function "and"(x, y : t) return t is
    begin
        if x = y then
            return A;
        else
            return B;
        end if;
    end function;

    function "and"(x, y : tv) return tv is
        variable tmp : tv(x'range);
    begin
        for i in x'range loop
            tmp(i) := x(i) and y(i);
        end loop;
        return tmp;
    end function;
    
begin

    process is
        variable x, y : t;
        variable xv, yv : tv(1 to 2);
    begin
        x := A;
        y := A;
        assert (x and y) = A;
        y := B;
        assert (x and y) = B;
        xv := (A, B);
        yv := (B, B);
        assert (xv and yv) = (B, A);
        wait;
    end process;
    
end architecture;
