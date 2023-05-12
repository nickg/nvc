entity condexpr is
end entity;

architecture test of condexpr is
    constant c1 : integer := 1 when 1 > 2 else 5;
begin
end architecture;
