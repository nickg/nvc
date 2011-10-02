entity signal3 is
end entity;

architecture test of signal3 is
    signal v : bit_vector(3 downto 0);
begin

    process is
    begin
        assert not v'active;
        assert not v'event;
        wait for 1 ns;
        v(2) <= '1';
        wait for 0 ns;
        assert v'event and v'active;
        wait;
    end process;
    
end architecture;
