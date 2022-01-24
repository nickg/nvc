entity access7 is
end entity;

architecture test of access7 is

    type int_ptr is access integer;
    type int_ptr_ptr is access int_ptr;
    type int_ptr_array is array (integer range <>) of int_ptr;
    type int_ptr_array_ptr is access int_ptr_array;

    procedure alloc_ptr(x : out int_ptr_ptr) is
    begin
        x := new int_ptr;
    end procedure;

    procedure alloc_ptr_array(x : out int_ptr_array_ptr) is
    begin
        x := new int_ptr_array(1 to 3);
    end procedure;

begin

    process is
        variable pp : int_ptr_ptr;
        variable pa : int_ptr_array_ptr;
    begin
        alloc_ptr(pp);
        assert pp.all = null;
        pp.all := new integer'(4);
        assert pp.all.all = 4;
        deallocate(pp.all);
        deallocate(pp);

        alloc_ptr_array(pa);
        assert pa.all = (null, null, null);
        pa(1) := new integer'(6);
        assert pa(1).all = 6;
        deallocate(pa(1));
        deallocate(pa);
        wait;
    end process;

end architecture;
