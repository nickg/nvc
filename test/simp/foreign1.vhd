entity foreign1 is
end entity;

architecture test of foreign1 is

    function f (x : integer) return integer;

    attribute foreign of f : function is "VHPIDIRECT symbol";

    function f (x : integer) return integer is
    begin
        return 99;                      -- Should not call this
    end function;

    function g (x : integer) return integer is
    begin
        return f(x) + 1;
    end function;

    constant c1 : integer := g(1);      -- Should not fold

begin
end architecture;
