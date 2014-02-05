entity sub is
    generic (
        WIDTH : integer;
        INIT  : bit );
    port (
        x : out bit_vector(31 downto 0);
        y : in  bit_vector(WIDTH - 1 downto 0) );
end entity;

architecture test of sub is
    signal y1 : bit_vector(WIDTH - 1 downto 0);
begin

    x <= (31 downto WIDTH => '0') & y1;

    y1 <= y;

end architecture;

-------------------------------------------------------------------------------

entity elab20 is
end entity;

architecture test of elab20 is
    signal x : bit_vector(31 downto 0);
    signal y : bit_vector(7 downto 0);
begin

    process is
    begin
        y <= X"55";
        wait for 1 ns;
        assert x = X"00000055";
        wait;
    end process;

    sub_i: entity work.sub
        generic map (
            WIDTH => 8,
            INIT  => '0' )
        port map ( x, y );

end architecture;
