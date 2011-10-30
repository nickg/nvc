entity wait4 is
end entity;

architecture test of wait4 is
    signal x, y, z : bit;
begin

    proc_a: process is
    begin
        wait for 1 ns;
        y <= '1';
        wait for 1 ns;
        z <= '1';
        wait for 1 ns;
        assert x = '1';
        wait;
    end process;

    proc_b: process is
    begin
        wait on x, y;
        assert y = '1';
        assert now = 1 ns;
        assert y'event report "not y'event";
        assert not x'event report "x'event";
        wait on z;
        assert not x'event;
        assert z'event;
        assert z = '1';
        x <= '1';
        wait;
    end process;
    
end architecture;
