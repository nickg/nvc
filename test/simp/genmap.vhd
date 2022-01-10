entity sub1 is
    generic (
        x : integer := 5;
        y : bit_vector(1 to 3) );
end entity;

entity genmap is
end entity;

architecture test of genmap is
begin

    u1: entity work.sub1
        generic map ( y => ('1', '1', '0'), x => 2 );

    u2: entity work.sub1
        generic map ( y => "101" );

    -- u3: entity work.sub1
    --     generic map (
    --         0, y(1) => '1', y(2) => '0', y(3) => '1' );

end architecture;
