entity sub1_ent is
    generic ( w : integer );
    port ( p : out bit_vector(1 to w) );
end entity;

architecture test of sub1_ent is
begin
end architecture;

-------------------------------------------------------------------------------

entity clone2 is
end entity;

architecture test of clone2 is
    component sub1 is
        generic ( w : integer );
        port ( p : out bit_vector(1 to w) );
    end component;

    signal s1, s2, s3 : bit_vector(1 to 5);

    for u1, u2 : sub1 use entity work.sub1_ent;
begin

    u1: component sub1
        generic map ( 5 )
        port map ( s1 );

    u2: component sub1                  -- Clone of u1
        generic map ( 5 )
        port map ( s2 );

    g1: for i in 1 to 3 generate        -- One copy
        p: s3(i) <= not s2(i);
    end generate;

end architecture;
