entity wait23 is
end entity;

architecture test of wait23 is

    type timerec is record
        t : time;
    end record;

    type timerec_vec is array (natural range <>) of timerec;

    signal s : integer := 0;
begin

    p1: process is
        variable a : timerec_vec(1 to 5) := ( (t => 1 ns),
                                              (t => 2 ns),
                                              (t => 3 ns),
                                              (t => 4 ms),
                                              (t => 5 ms) );
    begin
        for i in a'range loop
            s <= transport i after a(i).t;
        end loop;

        assert s = 0;
        wait for 1 ns;
        assert s = 1;
        wait for 1 ns;
        assert s = 2;
        wait for 1 ns;
        assert s = 3;
        wait for 4 ms;
        assert s = 4;
        wait for 1 ms;
        assert s = 5;

        wait;
    end process;

end architecture;
