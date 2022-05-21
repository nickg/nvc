-- Test garbage collection
entity access10 is
end entity;

architecture test of access10 is
    type rec;
    type rec_ptr is access rec;

    type rec is record
        s : string(1 to 50);
        i : integer;
        p : rec_ptr;
    end record;

    procedure do_test (p : inout rec_ptr) is
        variable n : rec_ptr;
    begin
        n := new rec;
        n.p := p;
        p := n;
    end procedure;

begin
    p1: process is
        variable p : rec_ptr;
    begin
        for i in 1 to 1000 loop
            for j in 1 to 10000 loop
                do_test(p);
            end loop;
            wait for 1 ns;
        end loop;
        wait;
    end process;

end architecture;
