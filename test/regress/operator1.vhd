entity operator1 is
end entity;

architecture test of operator1 is

    type t is (A, B);

    function "and"(x, y : t) return t is
    begin
        if x = y then
            return A;
        else
            return B;
        end if;
    end function;
    
begin

    process is
        variable x, y : t;
    begin
        x := A;
        y := A;
        assert (x and y) = A;
        y := B;
        assert (x and y) = B;
        wait;
    end process;
    
end architecture;
