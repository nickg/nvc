entity access5 is
end entity;

architecture test of access5 is
    type int_array is array (natural range <>) of integer;
    type int_array_ptr is access int_array;
begin

    process is
        variable a, b : int_array_ptr;
    begin
        a := new int_array(1 to 3);
        a.all := (1, 2, 3);
        b := new int_array'(a.all);
        assert b.all = (1, 2, 3);
        deallocate(a);
        deallocate(b);
        wait;
    end process;

end architecture;
