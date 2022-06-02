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
        x := ( (1, 2), z );             -- OK
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

    b2: block is
        signal vec : bit_vector(7 downto 0);
    begin
        vec <= (3 downto 0 => "111", others => '0'); -- OK (at parse time)
    end block;

    p3: process is
        type int_ptr is access integer;
        type int_ptr_array is array (integer range <>) of int_ptr;
        type int_ptr_array_ptr is access int_ptr_array;
        variable x : int_ptr_array_ptr;
    begin
        x.all := (null, null, null);     -- OK
    end process;

    p4: process is
        type int_vec2 is array (natural range <>) of integer_vector;  -- OK
        constant a : int_vec2(1 to 2)(1 to 2) := (  -- OK
            ((1, 2), (3, 4)), ((5, 6), (7, 8)) );
    begin
    end process;

    p5: process is
        type unit_spec_t is record
            name   : string(1 to 3);
            length : positive;
            unit   : time;
        end record;
        type unit_map_t is array (natural range <>) of unit_spec_t;
        constant unit_map : unit_map_t := (
            ( "fs ", 2, fs ),
            ( "ps ", 2, ps ) );         -- OK
    begin
    end process;

    p6: process is
        type t_bv_array is array (natural range <>) of bit_vector;
        subtype t_byte_array  is t_bv_array(open)(7 downto 0);  -- OK
        variable v1 : t_byte_array;     -- Error
        variable v2 : t_byte_array(1 to 3);  -- OK
        variable b3 : integer(open);    -- Error
        variable v3 : t_byte_array(1 to 3)(4 to 5);  -- Error
        subtype t_error1 is t_byte_array(1 to 3)(4 to 5);  -- Error
    begin
    end process;

end architecture;
