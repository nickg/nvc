entity agg2008 is
end entity;

architecture test of agg2008 is
begin

    p1: process is
        variable x : integer_vector(1 to 4);
        variable y, z : integer_vector(1 to 2);
    begin
        x := ( integer_vector'(1, 2), integer_vector'(3, 4) );  -- OK
        y := ( 5, 6 );                  -- OK
        z := ( 7, 8 );                  -- OK
        x := ( y, z );                  -- OK
        x := ( 1, 2, y, 3 );            -- OK
        z := ( 1, 2, 1.2 );             -- Error
        z := ( 1 => true, 2 => 5 );     -- Error
        x := ( (1, 2), z );             -- Error (but perhaps shouldn't be?)
        x := ( 1 => y, 2 => z );        -- Error
        x := ( 1 to 4 => x );           -- OK
        x := ( 1 to 2 => y, 3 to 4 => z );  -- OK
        wait;
    end process;

    p2: process is
        type int2d is array (natural range <>, natural range <>) of integer;
        variable x : int2d(1 to 2, 1 to 2);
    begin
        x := ( (1, 2), (3, 4) );        -- OK
        x := ( x, x );                  -- Error
    end process;

end architecture;
