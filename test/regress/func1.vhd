entity func1 is
end entity;

architecture test of func1 is
    function add1(x : integer) return integer is
    begin
        return x + 1;
    end function;
    
begin

    process is
        variable r : integer;
    begin
        r := add1(2);
        assert r = 3 report integer'image(r);
        wait;
    end process;
    
end architecture;
