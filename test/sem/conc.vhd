entity e is
end entity;

architecture a of e is
    signal x, y : integer;
begin

    x <= 4;                             -- OK

    y <= x + 2;                         -- OK

    x <= '4';                           -- Wrong type

    x <= 6 when y > 2 else 7;           -- OK

    x <= 7 when 7 else 3;               -- Condition not boolean

    x <= reject 5 inertial 7;           -- Reject not time

end architecture;
