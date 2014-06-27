entity toplevel1 is
    generic (
        WIDTH : integer := 6 );
    port (
        x : in bit_vector(WIDTH - 1 downto 0);
        y : out bit_vector(WIDTH - 1 downto 0) );
end entity;

architecture test of toplevel1 is
begin

    y <= x after 1 ns;

    process is
    begin
        assert x'length = 6;
        assert y'length = 6;
        assert x = "000000";
        wait;
    end process;

end architecture;
