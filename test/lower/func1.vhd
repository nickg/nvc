entity func1 is
end entity;

architecture test of func1 is
    function add1(x : integer) return integer is
    begin
        return x + 1;
    end function;

begin

    p1: process is
        variable r : integer;
    begin
        r := 2;
        r := add1(r);
        wait;
    end process;

end architecture;
