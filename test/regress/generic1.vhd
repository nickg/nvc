entity bot is
    generic ( constant N : integer );
    port ( x : out integer );
end entity;

architecture test of bot is
begin
    x <= N;
end architecture;

-------------------------------------------------------------------------------

entity generic1 is
end entity;

architecture test of generic1 is
    signal xa, xb : integer;
begin

    a: entity work.bot
        generic map ( 5 )
        port map ( xa );

    b: entity work.bot
        generic map ( 7 )
        port map ( xb );

    process is
    begin
        wait for 1 ns;
        assert xa = 5;
        assert xb = 7;
        wait;
    end process;
    
end architecture;

