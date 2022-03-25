entity record2008 is
end entity;

architecture test of record2008 is
    type rec1 is record
        x : bit_vector;                 -- OK
    end record;

    constant c1 : rec1 := ( x => "101" );  -- OK
    signal s1 : rec1;                   -- Error
    --signal r1 : rec1(x(1 to 3));        -- OK

begin

end architecture;
