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
--    signal r7 : rec2(b(1 to 3));        -- Error

begin

end architecture;
