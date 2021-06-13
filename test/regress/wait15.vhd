entity wait15 is
end entity;

architecture test of wait15 is
    type wait_spec_t is record
        delay : delay_length;
    end record;

    type wait_array_t is array (natural range <>) of wait_spec_t;

    constant wait_list : wait_array_t := ( (delay => 1 ns),
                                           (delay => 2 ns),
                                           (delay => 5 us ) );
begin

    process is
    begin
        for w in wait_list'range loop
            wait for wait_list(w).delay;
        end loop;
        assert now = 5003 ns;
        wait;
    end process;

end architecture;
