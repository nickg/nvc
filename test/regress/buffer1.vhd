entity sub is
    port ( x : buffer natural );
end entity;

architecture test of sub is
begin

    test: process is
    begin
        x <= 1;
        wait for 1 ns;
        x <= 2;
        wait for 1 ns;
        assert x = 2;
        wait;
    end process;

end architecture;

entity buffer1 is
end entity;

architecture test of buffer1 is
    signal x : natural;
begin

    uut: entity work.sub port map ( x );

    main: process is
    begin
        assert x = 0;
        wait for 1 ns;
        assert x = 1;
        wait for 1 ns;
        assert x = 2;
        wait;
    end process;

end architecture;
