entity elab1_bot is
    port (
        i : in integer;
        o : out integer );
end entity;

architecture test of elab1_bot is
begin

    process (i) is
    begin
        o <= i + 1;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity elab1_top is
end entity;

architecture test of elab1_top is
    signal x, y : integer;
begin

    uut: entity work.elab1_bot
        port map ( x, y );
    
end architecture;
