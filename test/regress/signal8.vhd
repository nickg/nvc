entity signal8 is
end entity;

architecture test of signal8 is
    type int_array is array (integer range <>) of integer;
    type int_array_Nx4 is array (integer range <>) of int_array(1 to 4);
    
    signal a : int_array_Nx4(1 to 4) := (
        1 => ( 1,  2,  3,  4  ),
        2 => ( 5,  6,  7,  8  ),
        3 => ( 9,  10, 11, 12 ),
        4 => ( 13, 14, 15, 16 ) );
begin

    process is
        variable b : int_array(1 to 4);
    begin
        a(1)(2) <= 99;
        wait for 1 ns;
        b := a(1);
        --for i in b'range loop
        --    report "b(" & integer'image(i) & ") = " & integer'image(b(i));
        --end loop;
        assert b = ( 1, 99, 3, 4 );
        assert a(1)(2) = 99;
        a(1) <= ( 21, 22, 23, 24 );
        wait for 1 ns;
        assert a(1)(1) = 21;
        assert a(1)(3) = 23;
        wait;
    end process;

end architecture;
