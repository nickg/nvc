entity attr5 is
end entity;

architecture test of attr5 is

    type int_vec is array (integer range <>) of integer;

    type int_vec_ptr is access int_vec;

begin

    process is
        variable p : int_vec_ptr;
    begin
        p := new int_vec(1 to 10);
        report integer'image(p.all'length);
        assert p.all'length = 10;
        deallocate(p);
        p := new int_vec(1 to 20);
        report integer'image(p'length);
        assert p'length = 20;
        wait;
    end process;

end architecture;
