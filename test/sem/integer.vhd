entity b is
end entity;

architecture a of b is
    type my_int1 is range 0 to 10;
    type my_int2 is range -20 to 30;
    signal x : my_int1 := 2;
begin

    process is
        variable z : my_int1;
    begin
        z := x;
    end process;
    
    process is
        variable y : my_int2;
    begin
        -- Should generate error as my_int1 and my_int2 incompatible
        y := x;                         
    end process;
    
end architecture;
