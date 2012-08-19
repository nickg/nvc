entity open_bot is
    port (
        i : in integer;
        o : out integer );
end entity;

architecture test of open_bot is
begin

    process (i) is
    begin
        o <= i + 1;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity open_top is
end entity;

architecture test of open_top is
    signal x : integer;
begin

    uut: entity work.open_bot
        port map ( x, open );
    
end architecture;
