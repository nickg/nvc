entity record10 is
end entity;

architecture test of record10 is

    type rec1 is record
        x, y : integer;
    end record;

    type rec2 is record
        x : integer;
        y : bit_vector(1 to 3);
    end record;

begin

    process is
        variable r1   : rec1;
        variable r2   : rec2;
        variable a, b : integer;
        variable c    : bit_vector(1 to 3);
    begin
        a := 1;
        b := 2;
        r1 := rec1'(x => a, y => b);
        assert r1.x = 1;
        assert r1.y = 2;
        r2 := rec2'(x => a, y => c);
        assert r2.x = 1;
        assert r2.y = "000";
        wait;
    end process;

end architecture;
