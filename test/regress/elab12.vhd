entity sub is
    generic (
        WIDTH : integer );
    port (
        x : in bit;
        y : out bit_vector(WIDTH - 1 downto 0) );
end entity;

architecture test of sub is
begin

    y <= (WIDTH - 1 downto 0 => x);

end architecture;

-------------------------------------------------------------------------------

entity elab12 is
end entity;

architecture test of elab12 is
    signal x1, x2 : bit;
    signal y1     : bit_vector(3 downto 0);
    signal y2     : bit_vector(4 downto 0);
begin

    sub2_i: entity work.sub
        generic map ( 5 )
        port map ( x2, y2 );

    sub1_i: entity work.sub
        generic map ( 4 )
        port map ( x1, y1 );

    process is
    begin
        x1 <= '0';
        x2 <= '1';
        wait for 1 ns;
        assert y1 = "0000";
        assert y2 = "11111";
        wait;
    end process;

end architecture;
