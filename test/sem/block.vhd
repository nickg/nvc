entity blk is
end entity;

architecture test of blk is
    signal y : integer;
begin

    b1: block is
        generic ( g1 : integer; g2 : bit := '1' );  -- OK
        generic map ( g1 => 5 );        -- OK
        port ( p1 : in integer );       -- OK
        port map ( p1 => y );           -- OK

        signal internal :  integer;
    begin
        internal <= p1 + g1;            -- OK
    end block;

    b2: block is                        -- Error (missing g1 in map)
        generic ( g1 : integer );
    begin
    end block;

end architecture;
