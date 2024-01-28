entity ent1 is
    generic ( g1 : integer; g2 : boolean := true );
    port ( p1 : in bit;
           p2 : out bit;
           p3 : in integer := 0 );
end entity;

architecture dummy of ent1 is
begin

end architecture;

configuration ent1cfg of ent1 is
    for dummy
    end for;
end configuration;

-------------------------------------------------------------------------------

entity test is
end entity;

architecture test of test is
    component comp1 is
        port ( p1 : in bit;
               p2 : out bit;
               p3 : in integer := 0 );
    end component;

    for u1 : comp1 use entity work.ent1  -- OK
        generic map ( g1 => 5 );

    for u2 : comp1 use entity work.ent1;  -- Error

    component comp2 is
        generic ( g3 : integer );
        port ( p4 : out bit );
    end component;

    for u3 : comp2 use entity work.ent1  -- OK
        generic map ( g1 => g3 )
        port map ( p1 => '0', p2 => p4 );

    for u4 : comp2 use entity work.ent1  -- Error
        port map ( p1 => '0', p2 => p4 );

    component comp3 is
        generic ( g2 : boolean := true; g1 : integer  );
        port ( p2 : out bit;
               p1 : in bit;
               p3 : in integer := 0 );
    end component;

    for u5 : comp3 use entity work.ent1;  -- OK

    for u6 : comp3 use configuration work.ent1cfg;  -- OK

    for u7 : comp3 use configuration work.ent1cfg
        generic map ( g1 => 5 );        -- OK (?)
begin

    u1: component comp1
        port map ( '1', open );

    u2: component comp1
        port map ( '1', open );

    u3: component comp2
        generic map ( g3 => 5 )
        port map ( open );

    u4: component comp2
        generic map ( g3 => 5 )
        port map ( open );

    u5: component comp3
        generic map ( g1 => 2 )
        port map ( open, '1' );

    u6: component comp3
        generic map ( g1 => 2 )
        port map ( open, '1' );

    u7: component comp3
        generic map ( g1 => 2 )
        port map ( open, '1' );

    u8: component comp3
        generic map ( g1 => 2 )
        port map ( open, '1' );

end architecture;
