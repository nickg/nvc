entity implicit1 is
end entity;

architecture test of implicit1 is
    signal x : natural;
begin

    x <= 1 after 1 ns,
         2 after 2 ns,
         3 after 3 ns;

    process is
    begin
        assert x = 0;
        assert x'delayed = 0;
        wait for 1 ns;
        assert x = 1;
        assert x'delayed = 0;
        wait for 0 ns;
        assert x'delayed = 1;
        assert x'delayed(1 ns) = 0;
        wait for 1 ns;
        assert x'delayed = 1;
        assert x'delayed(1 ns) = 1;
        wait;
    end process;

end architecture;
