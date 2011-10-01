entity signal2 is
end entity;

architecture test of signal2 is
    signal x : bit := '0';
begin

    process is
    begin
        assert not x'event report "a";
        assert not x'active report "b";
        wait for 1 ns;
        x <= '1';
        assert not x'event report "c";
        wait for 0 ns;
        assert x'event;
        assert x'active;
        wait for 0 ns;
        assert not x'event;
        wait for 1 ns;
        assert not x'event;
        assert not x'active;
        x <= '1';
        wait for 0 ns;
        assert not x'event;
        assert x'active;
        wait;
    end process;
    
end architecture;
