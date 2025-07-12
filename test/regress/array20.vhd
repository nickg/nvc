entity array20 is
end entity;

architecture test of array20 is
    type t_int2d is array (natural range <>, natural range <>) of integer;
    type t_int_int is array (natural range <>) of integer_vector;

    type t_rec is record
        x : integer;
        y : integer_vector;
    end record;

    type t_rec_array is array (natural range <>) of t_rec;
    type t_rec_array_array is array (natural range <>) of t_rec_array;

    function test_eq (x, y : t_int2d) return boolean is
    begin
        return x = y;
    end function;

    function test_eq (x, y : t_int_int) return boolean is
    begin
        return x = y;
    end function;

    function test_eq (x, y : t_rec_array_array) return boolean is
    begin
        return x = y;
    end function;
begin

    process is
        variable a1 : t_int2d(1 to 7, 2 to 5) := (others => (others => 0));
        variable a2 : t_int2d(2 to 5, 1 to 7) := (others => (others => 0));
        variable a3 : t_int_int(1 to 5)(2 to 3) := (others => (others => 0));
        variable a4 : t_int_int(2 to 3)(1 to 5) := (others => (others => 0));
        variable a5 : t_rec_array_array(1 to 2)(2 to 3)(y(1 to 7)) := (others => (others => (x => 0, y => (others => 0))));
        variable a6 : t_rec_array_array(1 to 2)(2 to 3)(y(1 to 5)) := (others => (others => (x => 0, y => (others => 0))));
    begin
        assert not test_eq(a1, a2);
        assert not test_eq(a3, a4);
        assert not test_eq(a5, a6);
        wait;
    end process;

end architecture;
