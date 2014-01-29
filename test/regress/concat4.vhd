entity concat4 is
end entity;

architecture test of concat4 is
    type mem_type is array (2 downto 0) of bit_vector(7 downto 0);
begin

    process is
        variable m : mem_type := ( X"03", X"02", X"01" );
        variable b : bit_vector(7 downto 0);
    begin
        b := X"ff";
        m := m(1 downto 0) & b;
        assert m = ( X"02", X"01", X"ff" );
        wait;
    end process;

end architecture;
