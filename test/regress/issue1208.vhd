entity issue1208 is
end entity;

architecture test of issue1208 is
    type bus_t is record
        valid : bit;
        ack   : bit;
    end record;

    view bus_master of bus_t is
        valid : out;
        ack   : in;
    end view;

    alias bus_slave is bus_master'converse;

    type doublebus_t is record
        slave, master : bus_t;
    end record;

    view double_master of doublebus_t is
        slave  : view bus_slave;
        master : view bus_master;
    end view;

    signal bs, bm : bus_t;
begin
    b : block is
        port (double : view double_master);
        port map (
            double.slave  => bs,
            double.master => bm
        );
    begin
        double.slave.ack <= double.slave.valid after 1 ps;
        double.master.valid <= double.master.ack after 1 ps;
    end block;

    check: process is
    begin
        wait for 1 ns;
        assert bm.valid = '0';
        assert bs.ack = '0';
        bm.ack <= '1';
        wait for 1 ns;
        assert bm.valid = '1';
        assert bs.ack = '0';
        bs.valid <= '1';
        wait for 1 ns;
        assert bm.valid = '1';
        assert bs.ack = '1';
        wait;
    end process;
end architecture;
