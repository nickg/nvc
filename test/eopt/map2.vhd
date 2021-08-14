entity sub is
    port (
        i : in  bit_vector(0 to 7);
        o : out bit_vector(0 to 7) );
end entity;

architecture test of sub is
begin

    o <= not i after 1 ns;

end architecture;

-------------------------------------------------------------------------------

entity map2 is
end entity;

architecture test of map2 is
    signal a : bit_vector(0 to 1);
    signal b : bit_vector(0 to 5);
    signal c : bit_vector(2 to 5);
    signal d : bit_vector(0 to 3);
begin

    sub1_i: entity work.sub
        port map (
            i(7) => '1',
            i(0 to 1) => a,
            i(2) => '0',
            i(3 to 6) => c,
            o => open );

end architecture;
