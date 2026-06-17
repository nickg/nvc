entity sub is
    port ( o : out integer );
end entity;

architecture test of sub is
begin
    o <= 1;
end architecture;

-------------------------------------------------------------------------------

entity top is
end entity;

architecture test of top is
    component sub is
        port ( o : out integer );
    end component;

    signal x, y : integer;
begin

    u1: component sub port map ( x );

    u2: component sub port map ( y );

    check: process is
    begin
        wait for 1 ns;
        assert x = 1;
        assert y = 1;
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

configuration conf6 of top is
    for test
        for u1 : sub
            use entity work.sub(test);
        end for;
    end for;
end configuration;
