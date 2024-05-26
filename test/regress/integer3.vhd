entity integer3 is
end entity;

architecture test of integer3 is
    type t_counter is protected
        procedure increment;
        impure function value return natural;
    end protected;

    type t_counter is protected body
        variable val : natural := 0;

        procedure increment is
        begin
            val := val + 1;
        end procedure;

        impure function value return natural is
        begin
            return val;
        end function;
    end protected body;

    shared variable count : t_counter;

    impure function get_right return integer is
    begin
        count.increment;
        return 42;
    end function;

begin

    process is
        variable x : integer range 1 to get_right;
        variable y : x'subtype;
    begin
        y := 5;
        x := y;
        assert x = 5;
        assert y'subtype'right = 42;
        assert count.value = 5;         -- XXX: should be 1
        wait;
    end process;

end architecture;
