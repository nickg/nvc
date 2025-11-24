entity sub is
    generic (
        I : integer;
        R : real := 1.5;
        S : string );
    port (
        p : in real );
end entity;

architecture test of sub is
begin

    process is
    begin
        assert I = integer'value(S);
        assert r = p;
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity toplevel3 is
end entity;

architecture test of toplevel3 is
    signal s : real := 1.5;
begin

    uut: entity work.sub
        generic map (4, s => "500")
        port map (s);

end architecture;
