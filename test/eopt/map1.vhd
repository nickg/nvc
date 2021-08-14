entity sub is
    port (
        i : in  bit_vector(7 downto 0);
        o : out bit_vector(7 downto 0) );
end entity;

architecture test of sub is
begin

    o <= not i after 1 ns;

end architecture;

-------------------------------------------------------------------------------

entity map1 is
end entity;

architecture test of map1 is
    signal a : bit_vector(1 downto 0);
    signal b : bit_vector(5 downto 0);
    signal c : bit_vector(5 downto 2);
    signal d : bit_vector(3 downto 0);
begin

    sub1_i: entity work.sub
        port map (
            i(1 downto 0) => a,
            i(7 downto 2) => b,
            o(3 downto 0) => c,
            o(7 downto 4) => d );

    sub2_i: entity work.sub
        port map (
            i(1 downto 0) => a,
            i(7 downto 2) => "000000",
            o => open );

end architecture;
