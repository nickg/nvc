entity access4 is
end entity;

architecture test of access4 is

    type int_vec is array (integer range <>) of integer;
    type int_vec_ptr is access int_vec;

begin

    process is
        variable p : int_vec_ptr;
    begin
        p := new int_vec(1 to 10);
        p(1 to 3) := (1, 2, 3);
        assert p(1 to 3) = (1, 2, 3);
        assert p(2) = 2;
        p.all(4 to 6) := (4, 5, 6);
        assert p.all(4) = 4;
        assert p'length = 10;
        assert p.all'low = 1;
        deallocate(p);
        wait;
    end process;

end architecture;
