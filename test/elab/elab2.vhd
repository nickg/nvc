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

entity elab2_top is
end entity;

architecture test of elab2_top is
    signal a, b, c : integer;
begin

    bot1: entity work.elab2_bot
        port map ( a, b );

    bot2: entity work.elab2_bot
        port map ( b, c );
    
end architecture;
