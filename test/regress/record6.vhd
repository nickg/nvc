entity record6 is
end entity;

architecture test of record6 is
    type rec is record
        x : bit_vector(1 to 3);
        y : integer;
    end record;

    type rec_array is array (natural range <>) of rec;

    function make_rec(x : bit_vector(1 to 3); y : integer) return rec is
        variable r : rec;
    begin
        r.x := x;
        r.y := y;
        return r;
    end function;

    function make_rec_array(x : rec; l, r : natural) return rec_array is
        variable ra : rec_array(l to r) := (others => x);
    begin
        return ra;
    end function;

    function get_bit(v : in rec) return bit is
    begin
        return v.x(v.y);
    end function;
begin

    process is
        variable r : rec;
        variable one : integer := 1;    -- Prevent constant folding
    begin
        r.x := "101";
        r.y := 1;
        assert get_bit(r) = '1';
        r.y := 2;
        assert get_bit(r) = '0';
        assert get_bit(make_rec("011", one + 1)) = '1';
        r.x := make_rec("010", one).x;
        assert r.x = "010";
        r.y := make_rec("010", one).y;
        assert r.y = 1;
        r := make_rec("010", one);
        assert make_rec_array(r, 1, 2) = ( ("010", 1), ("010", 1) );
        assert make_rec_array(("111", 5), one, 2) = ( ("111", 5), ("111", 5) );
        wait;
    end process;

end architecture;
