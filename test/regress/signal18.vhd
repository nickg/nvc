entity signal18 is
end entity;

architecture test of signal18 is
    signal x, y : integer;
begin

    p1: process (x, y) is
    begin
        x <= 1 when y > 10 else 0;
    end process;

    main: process is
    begin
        wait for 1 ns;
        assert x = 0;
        y <= 15;
        wait for 1 ns;
        assert x = 1;
        wait;
    end process;

end architecture;
