entity force1 is
end entity;

architecture test of force1 is
    signal x : bit;
    signal y : integer;
    signal z : bit_vector(1 to 3);
begin

    x <= '0';
    y <= 55;
    z <= "001";

    tb: process is
    begin
        wait for 1 ns;
        assert x = '0';
        assert y = 55;
        assert z = "001";
        wait for 1 ns;
        assert x = '1';
        assert y = 42;
        assert z = "110";
        wait for 1 ns;
        assert x = '0';
        assert y = 42;
        wait for 1 ns;
        assert y = 55;
        assert z = "001";
        wait;
    end process;

end architecture;
