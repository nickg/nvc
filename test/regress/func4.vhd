entity func4 is
end entity;

architecture test of func4 is

    type int_array is array (integer range <>) of integer;

    function cut(x : int_array; l, r : integer) return int_array is
    begin
        return x(l to r);
    end function;
    
begin

    process is
        variable a : int_array(1 to 4) := (1, 2, 3, 4);
        variable tmp : int_array(1 to 2);
    begin
        tmp := cut(a, 2, 3);
        assert tmp = (2, 3);
        wait;
    end process;

end architecture;
