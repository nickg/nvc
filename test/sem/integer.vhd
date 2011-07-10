architecture a of b is
    type my_int1 is range 0 to 10;
    type my_int2 is range -20 to 30;
    signal x : my_int1 := 2;
begin

    foo: process is
        variable y : my_int2;
    begin
        my_int2 := my_int1;
        my_int2 := my_int2 + (2 * my_int1);
    end process;
    
end architecture;
