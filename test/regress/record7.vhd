entity record7 is
end entity;

architecture test of record7 is
    type rec is record
        x, y : integer;
    end record;

    signal s : rec;
begin

    process is
    begin
        assert s = (integer'left, integer'left);
        s <= (1, 2);
        wait for 1 ns;
        assert s = (1, 2);
        assert s.x = 1;
        assert s.y = 2;
        s.x <= 3;
        s.y <= 4;
        wait for 1 ns;
        assert s = (3, 4);
        wait;
    end process;

end architecture;
