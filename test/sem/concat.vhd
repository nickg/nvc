entity concat is
end entity;

architecture t of concat is
    type int_array is array (integer range <>) of integer;
begin

    process
        variable w : int_array(1 to 4);
        variable x, y : int_array(1 to 3);
        variable z : int_array(1 to 6);
        variable s : string(1 to 5);
        variable t : int_array(1 to 2);
        variable b : bit_vector(1 to 3);
        variable c : bit_vector(1 to 4);
    begin
        x := ( 1, 2, 3 );
        y := ( 4, 5, 6 );

        z := x & y;                     -- OK
        w := 1 & x;                     -- OK
        w := y & 5;                     -- OK
        s := 'h' & string'("ello");     -- OK
        s := 1 & string'("ello");       -- Error
        t := 6 & 7;                     -- OK
        t := 7 & character'( 'x' );     -- Error
        c := bit_vector(b & '1');       -- OK
        wait;
    end process;

end architecture;
