entity sub is
    port (
        i : in integer;
        o : out integer );
end entity;

architecture test of sub is
begin
    o <= i;
end architecture;

-------------------------------------------------------------------------------

entity elab29 is
end entity;

architecture test of elab29 is
    signal x, y, z : integer := 0;
begin

    u: entity work.sub port map ( x + y, z );

    main: process is
    begin
        x <= 2;
        y <= 3;
        wait for 1 ns;
        assert z = 5;
        x <= 7;
        wait for 1 ns;
        assert z = 10;
        y <= -1;
        wait for 1 ns;
        assert z = 6;
        wait;
    end process;

end architecture;
