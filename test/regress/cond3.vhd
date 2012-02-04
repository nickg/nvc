entity cond3 is
end entity;

architecture test of cond3 is
    signal x, y, z : integer := 0;
begin

    x <= y + 1, y + 2 after 2 ns when z > 0 else 0;

    process is
    begin
        wait for 1 ns;
        assert x = 0;
        z <= 1;
        wait for 1 ns;
        assert x = 1;
        wait for 2 ns;
        assert x = 2;
        y <= 2;
        z <= 1;
        wait for 1 ns;
        assert x = 3;
        wait for 2 ns;
        assert x = 4;
        y <= 5;
        wait for 1 ns;
        assert x = 6;
        wait for 2 ns;
        assert x = 7;
        z <= 0;
        wait for 1 ns;
        assert x = 0;
        wait;
    end process;

end architecture;
