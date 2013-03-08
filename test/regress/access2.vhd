entity access2 is
end entity;

architecture test of access2 is

    type int_vec is array (integer range <>) of integer;

    type int_vec_ptr is access int_vec;

    subtype int_vec10 is int_vec(1 to 10);

    type int_vec10_ptr is access int_vec10;

    subtype one_to_3 is integer range 1 to 3;

begin

    process is
        variable p : int_vec_ptr;
        variable q : int_vec10_ptr;
        variable r : int_vec_ptr;
    begin
        p := new int_vec(1 to 5);
        p(1) := 2;
        assert p(1) = 2;
        deallocate(p);
        assert p = null;

        q := new int_vec10;
        q(3) := 5;
        assert q(3) = 5;
        deallocate(q);

        r := new int_vec(one_to_3'range);
        deallocate(r);

        wait;
    end process;

end architecture;
