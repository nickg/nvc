entity protected10 is
end entity;

architecture test of protected10 is

    type counter_t is protected
        procedure increment;
        procedure decrement;
        impure function value return natural;
    end protected;

    type counter_t is protected body
        variable val : natural := 0;

        procedure increment is
        begin
            val := val + 1;
        end procedure;

        procedure decrement is
        begin
            val := val - 1;
        end procedure;

        impure function value return natural is
        begin
            return val;
        end function;
    end protected body;

    type counter_ptr_t is access counter_t;

    procedure call_increment (p : inout counter_ptr_t; n : in natural) is
    begin
        for i in 1 to n loop
            p.increment;
        end loop;
    end procedure;
begin

    p1: process is
        variable p, p2 : counter_ptr_t;
    begin
        p := new counter_t;
        assert p.all.value = 0;
        p.increment;
        assert p.value = 1;
        call_increment(p, 5);
        assert p.value = 6;
        wait for 1 ns;
        for i in 1 to 10 loop
            -- Make sure the allocation of P was not in the thread local buffer
            p2 := new counter_t;
        end loop;
        assert p.value = 6;
        deallocate(p);
        assert p = null;
        wait;
    end process;

end architecture;
