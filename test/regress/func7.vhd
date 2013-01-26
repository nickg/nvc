entity func7 is
end entity;

architecture test of func7 is

    type int_vector is array (natural range <>) of integer;

    subtype int_vec_3 is int_vector(0 to 2);

    function get_ints(a, b, c : integer) return int_vec_3 is
    begin
        return int_vec_3'(a, b, c);
    end function;

begin

    process is
        variable v : int_vec_3;
    begin
        v := get_ints(1, 2, 3);
        assert v = (1, 2, 3);
        assert get_ints(4, 5, 6) = (4, 5, 6);
        wait;
    end process;

end architecture;
