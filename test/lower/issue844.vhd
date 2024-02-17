entity sub is
    generic ( g1 : natural; g2 : natural := g1 );
    port ( p : bit_vector(1 to g2) );
end entity;

architecture test of sub is
begin

end architecture;

-------------------------------------------------------------------------------

entity issue844 is
end entity;

architecture test of issue844 is
begin

    u: entity work.sub
        generic map ( g1 => 4 )
        port map ( p => (others => '0') );

end architecture;
