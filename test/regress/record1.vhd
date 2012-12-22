entity record1 is
end entity;

architecture test of record1 is

    type r1 is record
        x, y : integer;
    end record;

begin

    process is
        variable a : r1;
        variable b : r1 := (1, 2);
        variable c : r1 := (x => 10, y => 2);
        variable d : r1 := (others => 99);
    begin
        assert a.x = integer'left;
        assert (b.x = 1) and (b.y = 2);
        assert (c.x / c.y) = 5;
        assert (d.x = 99) and (d.y = 99);
        wait;
    end process;

end architecture;
