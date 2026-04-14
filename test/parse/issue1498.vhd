entity issue1498 is
end entity;

architecture test of issue1498 is

    function bit_width(x : integer) return integer is
    begin
        return x;
    end;

    function f(x : natural) return natural is
    begin
        return x;
    end;

    function g(
        bit_width : natural := 0
    ) return integer is
    begin
        return 0;
    end;

    constant c : integer := g(bit_width => f(3));

begin
end architecture;
