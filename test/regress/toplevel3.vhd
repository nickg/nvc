entity sub is
    generic (
        I : integer;
        S : string );
end entity;

architecture test of sub is
begin

    process is
    begin
        assert I = integer'value(S);
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity toplevel3 is
end entity;

architecture test of toplevel3 is
begin

    uut: entity work.sub
        generic map (4, "500");

end architecture;
