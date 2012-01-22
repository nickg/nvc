entity agg1 is
end entity;

architecture test of agg1 is
    type int_array is array (integer range <>) of integer;
begin

    process is
        variable x : integer;
        variable v : int_array(1 to 3);
    begin
        x := 5;
        v := ( 1, x, 2 );
        assert v = ( 1, 5, 2 );
        v := ( v(3), v(2), v(1) );
        assert v = ( 2, 5, 1 );
        wait;
    end process;
    
end architecture;
