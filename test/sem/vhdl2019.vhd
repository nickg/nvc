entity vhdl2019 is
end entity;

architecture test of vhdl2019 is
    impure function func1 (variable x : inout integer) return integer is
    begin
        x := x + 1;
        return x;
    end function;
begin

end architecture;
