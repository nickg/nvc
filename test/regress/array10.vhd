entity array10 is
end entity;

architecture test of array10 is

    type int_vec is array (natural range <>) of integer;
    type int_vec_ptr is access int_vec;

    procedure do_stuff (variable p : inout int_vec_ptr;
                        variable r : inout integer) is
        constant orig : int_vec(1 to p'length) := p.all;
        constant len : integer := p'length;
    begin
        deallocate(p);
        p := new int_vec'(1, 2, 3);     -- Changes p'length
        r := 0;
        for i in 1 to len loop
            r := r + orig(i);
        end loop;
    end procedure;

begin

    p1: process is
        variable p : int_vec_ptr;
        variable r : integer;
    begin
        p := new int_vec'(1, 2, 3, 4, 5);
        do_stuff(p, r);
        assert r = 15;
        assert p.all = (1, 2, 3);
        wait;
    end process;

end architecture;
