entity visibility13 is
end entity;

architecture test of visibility13 is
    signal s : integer := 0;

    component c is
        generic ( g : integer );
        port ( p : in integer);
    end component;
begin
    b: block is
        generic ( g : integer );
        generic map ( g => 2 );
        port ( p : in integer);
        port map ( p => s + g );        -- Error (g not visibile here)
    begin
    end block;

    u: component c
        generic map ( g => 2 )
        port map ( p => s + g );        -- Error (g not visibile here)

end architecture;
