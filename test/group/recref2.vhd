entity recref2 is
end entity;

architecture test of recref2 is
    type rec1 is record
        x, y : bit_vector(1 to 2);
    end record;

    type rec2 is record
        a, b : rec1;
    end record;

    type rec3 is record
        x : integer;
    end record;

    type rec4 is record
        a, b : rec3;
    end record;

    signal r : rec2;
    signal s : rec4;
begin

end architecture;
