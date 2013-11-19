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

    function get_bit(v : in rec) return bit is
    begin
        return v.x(v.y);
    end function;
begin

    process is
        variable r : rec;
    begin
        r.x := "101";
        r.y := 1;
        assert get_bit(r) = '1';
        r.y := 2;
        assert get_bit(r) = '0';
        assert get_bit(make_rec("011", 2)) = '1';
        r.x := make_rec("010", 1).x;
        assert r.x = "010";
        --r.y := make_rec("010", 1).y;
        --assert r.y = 1;
        --r := make_rec("010", 1);
        wait;
    end process;

end architecture;
