entity record2008 is
end entity;

architecture test of record2008 is
    type rec1 is record
        x : bit_vector;                 -- OK
    end record;

    constant c1 : rec1 := ( x => "101" );  -- OK
    signal s1 : rec1;                   -- Error
    signal r1 : rec1(x(1 to 3));        -- OK
    signal r2 : rec1(y(1 to 3));        -- Error
    signal r3 : rec1(r2(1 to 2));       -- Error

    type rec2 is record
        x, y : bit_vector;              -- OK
        a : bit_vector(1 to 3);
        b : integer;
    end record;

    signal r4 : rec2(x(1 to 3), y(1 to 4));  -- OK
    signal r5 : rec2(x(1 to 3), x(1 to 4));  -- Error
    signal r6 : rec2(x(1 to 3), p(1 to 4));  -- Error
    signal r7 : rec2(b(1 to 3));        -- Error
    signal r8 : rec2(x(1 to 3));        -- Error

    subtype t1 is rec1(x(1 to 3));      -- OK
    signal r9 : t1;                     -- OK
    subtype t2 is rec1;                 -- OK
    signal r10 : t2;                    -- Error
    subtype t3 is rec2(x(1 to 5));      -- OK
    signal r11 : t3(y(1 to 2));         -- OK
    subtype t4 is t3(y(1 to 1));        -- OK
    subtype t5 is t4(x(1 to 2));        -- Error

    type rec3 is record
        r : rec1;                       -- OK
        s : bit_vector;                 -- OK
    end record;

    signal r12 : rec3;                  -- Error
    signal r13 : rec3(r(x(1 to 3)), s(1 to 2));  -- OK
    signal r14 : rec3(s(1 to 2));       -- Error
begin

end architecture;
