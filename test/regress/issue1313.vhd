entity issue1313 is
end entity;

architecture test of issue1313 is
    type t_complex is record
        r, i : real;
    end record;

    type t_rec is record
        x : integer;
        y : bit;
        z : t_complex;
    end record;

    signal s : t_rec := (1, '1', (1.5, 2.0));
begin

    process is
    begin
        s <= force (5, '0', (0.5, 6.0));
        wait for 1 ns;
        assert s.x = 5;
        assert s.z.i = 6.0;
        s <= release;
        wait for 1 ns;
        assert s.x = 1;
        assert s.z.i = 2.0;
        wait;
    end process;

end architecture;
