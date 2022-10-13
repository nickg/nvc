entity top is
end entity;

architecture test of top is
    component sub1 is
    end component;

    component sub2 is
        port ( p : integer );
    end component;
begin

    u1: component sub1;

    u2: component sub2
        port map ( p => 3 );

end architecture;

-------------------------------------------------------------------------------

entity e1 is
end entity;

architecture test of e1 is
begin

end architecture;

-------------------------------------------------------------------------------

entity e2 is
    port ( p : in integer );
end entity;

architecture test of e2 is
begin

end architecture;

-------------------------------------------------------------------------------

configuration config1 of top is
    for test
        for u1 : sub1
            use entity work.e1(test);
        end for;
        for all : sub2
            use entity work.e2(test);
        end for;
    end for;
end configuration;
