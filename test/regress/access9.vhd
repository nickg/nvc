-- Test garbage collection
entity access9 is
end entity;

architecture test of access9 is
    type int_vec is array (natural range <>) of integer;
    type int_vec_ptr is access int_vec;

    type rec is record
        s : string(1 to 50);
        i : integer;
        p : int_vec_ptr;
    end record;

    type rec_ptr is access rec;

    procedure do_test (p : inout rec_ptr) is
    begin
        p := new rec;
        p.p := new int_vec(1 to 1000);
    end procedure;

begin
    p1: process is
        variable p : rec_ptr;
    begin
        for i in 1 to 1000 loop
            for j in 1 to 100 loop
                do_test(p);
            end loop;
            wait for 1 ns;
        end loop;
        wait;
    end process;

end architecture;
