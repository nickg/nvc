entity initial is
end entity;

architecture test of initial is
    type real_vector is array (natural range <>) of real;
    constant c1 : real_vector := (1.0, 0.0, 1.0, 0.0, 1.0);
    constant c2 : real_vector(1 to c1'length) :=
        (0.0, 1.0, 0.0, 1.0, c1(2), 0.0);  -- Error
    signal s1 : real_vector := (1.0, 0.0, 1.0, 0.0, 1.0);
    signal s2 : real_vector(1 to s1'length) :=
        (0.0, 1.0, 0.0, 1.0, c1(2), 0.0);  -- Error
begin

    p1: process is
        variable v1 : real_vector := (1.0, 0.0, 1.0, 0.0, 1.0);
        variable v2 : real_vector(1 to v1'length) :=
            (0.0, 1.0, 0.0, 1.0, 1.0, 0.0);  -- Error
    begin
        wait for 5 ns;
        assert false;
    end process;

end architecture;
