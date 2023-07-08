entity condexpr is
end entity;

architecture test of condexpr is
    constant c1 : integer := 1 when 1 > 2 else 5;

    function foo (x : integer) return integer is
    begin
        return 2 when x > 1 else unaffected;
        return 5 when x < 5 else 7;
        return 7 when x = 2;
    end function;

    procedure bar (x : integer) is
    begin
        return when x = 2;
        return when 2 = 3;
        return when 1 = 1;
    end procedure;
begin
end architecture;
