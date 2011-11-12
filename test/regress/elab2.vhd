entity elab2_bot is
    port (
        i : in integer;
        o : out integer );
end entity;

architecture test of elab2_bot is
begin

    process (i) is
    begin
        o <= i + 1;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity elab2 is
end entity;

architecture test of elab2 is
    signal a, b, c : integer;
begin

    bot1: entity work.elab2_bot
        port map ( a, b );

    bot2: entity work.elab2_bot
        port map ( b, c );

    process is
    begin
        a <= 0;
        wait for 1 ns;
        assert b = 1;
        assert c = 2;
        a <= 2;
        wait for 1 ns;
        assert b = 3;
        assert c = 4;
        wait;
    end process;
    
end architecture;
