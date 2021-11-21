entity protected6 is
end entity;

architecture test of protected6 is

    type p is protected
        impure function sum return integer;
    end protected;

    type p is protected body
        type int_vector is array (natural range <>) of integer;
        variable v : int_vector(1 to 2 ** 20) := (others => 1);
        variable q : int_vector(1 to 500) := (others => 2);

        impure function sum return integer is
            variable r : integer := 0;
        begin
            for i in v'range loop
                r := r + v(i);
            end loop;
            for i in q'range loop
                r := r + q(i);
            end loop;
            return r;
        end function;
    end protected body;

    procedure run_test is
        -- Quickly exhausts memory if not cleaned up
        variable x : p;
    begin
        assert x.sum = (2 ** 20) + (500 * 2);
    end procedure;

begin

    process is
    begin
        for i in 1 to 100 loop
            run_test;
        end loop;
        wait;
    end process;

end architecture;
