entity record6 is
end entity;

architecture test of record6 is
    type rec is record
        x : bit_vector(1 to 3);
        y : integer;
    end record;

    function make_rec(x : bit_vector(1 to 3); y : integer) return rec is
        variable r : rec;
    begin
        r.x := x;
        r.y := y;
        return r;
    end function;
begin

end architecture;
