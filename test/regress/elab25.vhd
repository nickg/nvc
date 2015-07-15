entity sub is
    port (
        a : in  bit_vector(2 downto 0);
        b : out bit_vector(2 downto 0) );
end entity;

architecture test of sub is
begin
    b <= a after 0 ns;
end architecture;

-------------------------------------------------------------------------------

entity elab25 is
end entity;

architecture test of elab25 is
    signal x0, x1 : bit_vector(1 downto 0);
    signal y0, y1 : bit;
begin

    sub_i: entity work.sub
        port map (
            a(1 downto 0) => x0,
            a(2) => y0,
            b(1 downto 0) => x1,
            b(2) => y1 );

end architecture;
