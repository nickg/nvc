entity real2 is
end entity;

architecture test of real2 is
    type real_vec is array (integer range <>) of real;

    type real_rec is record
        x, y : real;
    end record;
begin

    process is
        variable a, b : real_vec(1 to 3);
        variable r : real_rec;
    begin
        a := (1.0, 1.2, 3.4);
        b := (0.9, 0.2, 4.1);
        assert a /= b;
        r.x := 2.0;
        r.y := 3.0;
        assert r = (2.0, 3.0);
        wait;
    end process;

end architecture;
