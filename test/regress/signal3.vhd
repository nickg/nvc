entity signal3 is
end entity;

architecture test of signal3 is
    signal v : bit_vector(3 downto 0);
begin

    proc1: process is
    begin
        assert not v'active;
        assert not v'event;
        wait for 1 ns;
        v(2) <= '1';
        wait for 0 ns;
        assert v'event and v'active;
        wait;
    end process;

    proc2: process is
    begin
        wait for 1 ns;
        wait for 0 ns;
        assert v'event and v'active;
        wait for 1 ns;
        v(0) <= '1';
        assert not v'event;
        wait for 0 ns;
        assert v'event and v'active;
        wait for 1 ns;
        assert not v'event;
        v(0) <= '1';
        wait for 0 ns;
        assert not v'event and v'active;
        wait;
    end process;
    
end architecture;
