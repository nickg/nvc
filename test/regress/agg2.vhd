entity agg2 is
end entity;

architecture test of agg2 is
    
    type int_array is array (integer range <>) of integer;

    function all_ones(x : int_array) return int_array is
        variable y : int_array(1 to x'length) := (others => 0);
    begin
        y := (others => 1);
        return y;
    end function;
    
begin

    process is
        variable x : int_array(1 to 3) := (others => 5);
    begin
        assert all_ones(x) = (1, 1, 1);
        wait;
    end process;    
    
end architecture;
