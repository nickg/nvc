entity record12 is
end entity;

architecture test of record12 is
    type rec is record
        a, b : integer;
    end record;

    type rec2 is record
        x, y : rec;
    end record;

    constant r1 : rec := (1, 2);
    constant r2 : rec := (3, 4);
    constant r3 : rec2 := (r1, r2);
begin

    process is
    begin
        assert r1.a = 1;
        assert r3.x = r1;
        assert r3.y = r2;
        assert r3.y.a = 3;
        wait;
    end process;

end architecture;
