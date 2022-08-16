entity func21 is
end entity;

architecture test of func21 is
    type rec is record
        x, y : integer;
    end record;

    function func (r : rec) return integer is
    begin
        return r.x + r.y;
    end function;
begin

    p1: process is
        variable a, b : integer;
    begin
        assert func(r.x => 1, r.y => 2) = 3;
        a := 4;
        b := 5;
        wait for 1 ns;
        assert func(r.x => a, r.y => b) = 9;
        wait;
    end process;

end architecture;
