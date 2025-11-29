entity sub1_ent is
    generic ( w : integer; v : real );
    port ( p : out bit_vector(1 to w) );
end entity;

architecture test of sub1_ent is
begin
    proc: p <= (others => '1');
end architecture;

-------------------------------------------------------------------------------

entity clone2 is
end entity;

architecture test of clone2 is
    component sub1 is
        generic ( w : integer; v : real );
        port ( p : out bit_vector(1 to w) );
    end component;

    component sub2 is
        generic ( type t );
        port ( p : out t );
    end component;

    component sub3 is
        generic ( x, y : integer );
        port ( p : out bit_vector(x - 1 downto 0) );
    end component;

    signal s1, s2, s3, s6, s7 : bit_vector(1 to 5);
    signal s4 : bit_vector(3 downto 0);
    signal s5 : bit_vector(7 downto 0);

    for u1, u2 : sub1 use entity work.sub1_ent;
    for u3, u4 : sub2 use open;
    for u5, u6 : sub3 use open;
begin

    u1: component sub1
        generic map ( 5, 1.2 )
        port map ( s1 );

    u2: component sub1                  -- Clone of u1
        generic map ( 5, 1.2 )
        port map ( s2 );

    g1: for i in 1 to 3 generate        -- One copy
        p: s3(i) <= not s2(i);
    end generate;

    u3: component sub2
        generic map ( integer );

    u4: component sub2                  -- Clone of u3
        generic map ( integer );

    u5: component sub3
        generic map ( 4, 8 )
        port map ( s4 );

    u6: component sub3
        generic map ( 8, 4 )
        port map ( s5 );

    u7: entity work.sub1_ent           -- Clone of u7.sub1_ent
        generic map ( 5, 1.2 )
        port map ( s6 );
end architecture;
