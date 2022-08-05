entity sub is
    generic ( g1 : integer; g2 : integer := g1 + 1 );
end entity;

architecture test of sub is
begin
    p1: assert g2 = g1 + 1;
end architecture;

-------------------------------------------------------------------------------

entity comp4 is
end entity;

architecture test of comp4 is
    component sub is
        generic ( g1 : integer; g2 : integer := g1 + 1 );
    end component;
begin

    u: component sub generic map ( 5 );

end architecture;
