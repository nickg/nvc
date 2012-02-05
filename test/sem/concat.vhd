entity concat is
end entity;

architecture t of concat is
    type int_array is array (integer range <>) of integer;
begin

    process
        variable x, y : int_array(1 to 3);
        variable z : int_array(1 to 6);
    begin
        x := ( 1, 2, 3 );
        y := ( 4, 5, 6 );

        z := x & y;                     -- OK
        
        wait;
    end process;

end architecture;

    
