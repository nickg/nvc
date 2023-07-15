entity signal33 is
end entity;

architecture test of signal33 is
    signal x : bit := '0';
begin

    p1: process is
    begin
        assert not x'event;
        assert not x'active;
        x <= '1';
        wait for 0 ns;
        assert x'event;
        assert x'active;
        x <= '1';
        wait for 0 ns;
        assert not x'event;
        assert x'active;
        wait;
    end process;

end architecture;
