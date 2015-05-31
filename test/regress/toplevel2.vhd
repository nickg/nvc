entity toplevel2 is
    generic (
        I : integer;
        S : string );
end entity;

architecture test of toplevel2 is
begin

    process is
    begin
        assert I = integer'value(S);
        wait;
    end process;

end architecture;
