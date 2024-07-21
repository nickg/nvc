entity implicit8 is
    generic ( tdelay : delay_length := 5 ps );
end entity;

architecture test of implicit8 is
    signal s : natural;
begin

    gen: process is
    begin
        for i in 1 to 10 loop
            s <= s + 1;
            wait for 1 ps;
        end loop;
        wait;
    end process;

    check1: process is
    begin
        wait for 9 ps;
        assert s = 9;
        assert s'delayed = 9;
        assert s'delayed(tdelay) = 5;
        wait for 0 ns;
        assert s = 10;
        assert s'delayed = 9;
        assert s'delayed(tdelay) = 5;
        wait for 0 ns;
        assert s = 10;
        assert s'delayed = 10;
        assert s'delayed(tdelay) = 5;

        wait;
    end process;

    check2: process is
    begin
        wait for 9 ps;
        assert s = 9;
        assert s'delayed = 9;
        assert s'delayed(tdelay) = 5;
        wait for 0 ns;
        assert s = 10;
        assert s'delayed = 9;
        assert s'delayed(tdelay) = 5;
        wait for 0 ns;
        assert s = 10;
        assert s'delayed = 10;
        assert s'delayed(tdelay) = 5;

        wait;
    end process;

end architecture;
