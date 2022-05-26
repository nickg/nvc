entity record32 is
end entity;

architecture test of record32 is
    type rec1 is record
        f1 : integer_vector;
    end record;

    type rec2 is record
        r : rec1;
        f2 : integer_vector;
    end record;

    signal s : rec2(f2(1 to 3), r(f1(2 to 5)));
begin

end architecture;
