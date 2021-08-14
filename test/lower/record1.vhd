entity record1 is
end entity;

architecture test of record1 is

    type r1 is record
        x, y : integer;
    end record;

begin

    p1: process is
        variable a, b : r1 := (1, 2);
    begin
        assert a.x = 1;
        a.x := 5;
        a := b;
        assert a.x = 1;
        assert a = b;
        wait;
    end process;

end architecture;
