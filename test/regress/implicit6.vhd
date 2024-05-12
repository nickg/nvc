entity implicit6 is
end entity;

architecture test of implicit6 is
    type t_rec is record
        x, y : integer;
    end record;

    signal x : t_rec;
begin

    x <= (1, 1), (2, 2) after 2 ns, (3, 3) after 4 ns;

    process is
        variable t0, t1 : bit;
    begin
        t0 := x'transaction;
        report bit'image(t0);
        wait for 1 ns;
        t1 := x'transaction;
        report bit'image(t1);
        assert t0 = not t1;
        wait for 2 ns;
        t0 := x'transaction;
        report bit'image(t0);
        assert t0 = not t1;
        wait for 2 ns;
        t1 := x'transaction;
        report bit'image(t1);
        assert t0 = not t1;
        wait;
    end process;

end architecture;
