entity issue1137 is
end entity;

architecture test of issue1137 is
    type t_rec is record
        x : integer;
        y : bit_vector(1 to 0);         -- Null
        z : integer;
    end record;

    signal s : t_rec := (42, "", 55);
begin

    process is
        variable v : t_rec := (666, "", 12);
    begin
        assert s.x = 42;
        assert s.y = "";
        assert s.z = 55;
        v.y := "";
        s.y <= "";
        wait for 1 ns;
        assert s.x = 42;
        assert s.y = v.y;
        assert s.z = 55;
        assert v.x = 666;
        assert v.y = "";
        assert v.z = 12;
        wait;
    end process;

end architecture;
