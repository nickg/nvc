entity protected12 is
end entity;

architecture test of protected12 is

    type t_counter is protected
        procedure increment;
        impure function get return natural;
    end protected;

    type t_counter is protected body
        variable count : natural;

        procedure increment is
        begin
            count := count + 1;
        end procedure;

        impure function get return natural is
        begin
            return count;
        end function;
    end protected body;

    type t_test is protected
        private variable c1 : t_counter;
        private variable c2 : t_counter;
        alias inc1 is c1.increment [];
        alias inc2 is c2.increment [];
        alias get1 is c1.get [return natural];
        alias get2 is c2.get [return natural];
        impure function sum return natural;
    end protected;

    type t_test is protected body
        impure function sum return natural is
        begin
            return get1 + get2;
        end function;
    end protected body;

    shared variable sv : t_test;
begin

    tb: process is
    begin
        sv.inc1;
        sv.inc2;
        wait for 1 ns;
        assert sv.get1 = 1;
        sv.inc2;
        assert sv.sum = 3;
        wait;
    end process;

end architecture;
