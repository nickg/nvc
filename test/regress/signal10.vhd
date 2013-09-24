entity signal10 is
end entity;

architecture test of signal10 is
    signal x, y, z : bit;
    signal u       : bit_vector(1 to 3);
    signal v       : bit_vector(2 downto 0);
begin

    process is
    begin
        (x, y, z) <= bit_vector'("011");
        wait for 1 ns;
        assert x = '0';
        assert y = '1';
        assert z = '1';

        u <= bit_vector'("011");
        wait for 1 ns;
        (x, y, z) <= u;
        wait for 1 ns;
        assert x = '0';
        assert y = '1';
        assert z = '1';

        v <= bit_vector'("011");
        wait for 1 ns;
        (x, y, z) <= v;
        wait for 1 ns;
        assert x = '0';
        assert y = '1';
        assert z = '1';

        wait;
    end process;

end architecture;
