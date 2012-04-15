entity sub is
    port (
        x, y : in integer;
        z    : out integer );
end entity;

architecture test of sub is
begin

    z <= x + y;

end architecture;

-------------------------------------------------------------------------------

entity elab4 is
end entity;

architecture test of elab4 is
    signal x1, z1, z2 : integer;
begin

    sub1_i: entity work.sub
        port map (
            x => x1,
            y => 2,
            z => z1 );

    sub2_i: entity work.sub
        port map (
            x => 6 + 15,
            y => 2 * 4,
            z => z2 );

    process is
    begin
        x1 <= 5;
        wait for 1 ns;
        assert z1 = 7;
        assert z2 = 29;
        wait;
    end process;

end architecture;
