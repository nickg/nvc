package p is

    type int_array is array (integer range <>) of integer;

    type ten_ints is array (1 to 10) of integer;
    
end package;

architecture a of e is
    signal x : int_array(1 to 5);
    signal y : ten_ints;
    signal z : int_array(1 to 3) := ( 0, 1, 2 );
    signal n : int_array(1 to 3) := ( 0, 1 => 1, others => 2 );
    signal m : int_array(1 to 3) := ( 1 to 3 => 0 );
begin

    process is
    begin
        x(0) <= 1;
        y(2) <= n(2);
        y(3)(5) <= n(2)(1);
    end process;

end architecture;
