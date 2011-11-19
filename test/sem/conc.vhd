entity e is
end entity;

architecture a of e is
    signal x, y : integer;
begin

    x <= 4;                             -- OK

    y <= x + 2;                         -- OK

    x <= '4';                           -- Wrong type

end architecture;
