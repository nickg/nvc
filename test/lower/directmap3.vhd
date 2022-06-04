entity sub is
    generic ( W : natural );
    port ( o : out bit_vector(1 to 2);
           p : in bit_vector(1 to W) );
end entity;

architecture test of sub is
begin
end architecture;

-------------------------------------------------------------------------------

entity directmap3 is
end entity;

architecture test of directmap3 is
    constant K : natural := 4;
    signal o1, o2 : bit;
    signal p : bit_vector(1 to 3);
begin

    u: entity work.sub
        generic map ( K - 1 )
        port map ( o(1) => o1, o(2) => o2, p => p );

end architecture;
