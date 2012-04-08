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
    
    with y select x <=                  -- OK
        1 when 3,
        2 when 8,
        3 when others;

    with y select x <=
        1 when y;                       -- Not locally static

    with y select x <=
        true when 5;                    -- Wrong type
    
    with y select x <=
        6, 7 after 1 ns when false;     -- Wrong type
    
end architecture;
