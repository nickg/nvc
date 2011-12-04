entity slice1 is
end entity;

architecture test of slice1 is
    type int_vector is array (integer range <>) of integer;
    signal x : int_vector(0 to 3);
begin

    process is
        variable v : int_vector(0 to 3);
    begin
        v := ( 1, 2, 3, 4 );
        v(1 to 2) := ( 6, 7 );
        assert v = ( 1, 6, 7, 4 );

        x <= ( 1, 2, 3, 4 );
        wait for 1 ns;
        x(1 to 2) <= ( 6, 7 );
        wait for 1 ns;
        assert x = ( 1, 6, 7, 4 );

        wait;
    end process;
    
end architecture;
