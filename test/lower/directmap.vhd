entity bot is
    port (
        i : in integer;
        o : out integer );
end entity;

architecture test of bot is
begin

    process (i) is
    begin
        o <= i + 1;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity directmap is
end entity;

architecture test of directmap is
    signal x, y : integer;
begin

    uut: entity work.bot
        port map ( x, y );

    process is
    begin
        x <= 0;
        wait for 1 ns;
        assert y = 1;
        x <= 2;
        wait for 1 ns;
        assert y = 3;
        wait;
    end process;

end architecture;
