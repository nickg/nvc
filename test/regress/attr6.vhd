entity attr6 is
end entity;

architecture test of attr6 is
    signal x : integer := 5;
    signal y : bit_vector(0 to 3);
    signal z : bit;                     -- No drivers
begin

    process is
    begin
        assert x'last_event = time'high;
        assert z'last_event = time'high;
        x <= 0;
        assert x'last_value = x;
        assert x'last_value = 5;
        wait for 1 ns;
        assert x'last_value = 5;
        assert x'last_event = 1 ns;
        x <= 2;
        wait for 1 ns;
        assert x = 2;
        assert x'last_value = 0;
        assert x'last_event = 1 ns;

        assert y'last_value = y;
        y <= ( '0', '1', '0', '1' );
        wait for 1 ns;
        assert y'last_value = ( '0', '0', '0', '0' );
        y(1) <= '1';
        wait for 1 ns;
        assert y'last_value = ( '0', '0', '0', '0' );
        y(1) <= '0';
        wait for 1 ns;
        assert y'last_value = ( '0', '1', '0', '0' );

        wait;
    end process;

end architecture;
