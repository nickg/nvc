entity access2 is
end entity;

architecture test of access2 is

    type int_vec is array (integer range <>) of integer;

    type int_vec_ptr is access int_vec;

begin

    process is
        variable p : int_vec_ptr;
    begin
        p := new int_vec(1 to 5);
        p(1) := 2;
        assert p(1) = 2;
        deallocate(p);
        --assert p = null;
        wait;
    end process;

end architecture;
