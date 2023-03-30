entity record38 is
end entity;

architecture test of record38 is

    type rec1_t is record
        x : integer_vector;
    end record;

    type rec1_array is array (natural range <>) of rec1_t;

    type rec2_t is record
        a : rec1_array;
    end record;
begin

    p1: process is
        variable v : rec2_t(a(1 to 2)(x(1 to 3)));
        constant k : rec2_t(a(1 to 2)(x(1 to 3))) := (a => (others => (x => (100, 200, 300))));
    begin
        assert v.a(1).x(1) = integer'left;
        v.a(1).x := (1, 2, 3);
        wait for 1 ns;
        assert v.a(1).x(2) = 2;

        v := (a => k.a);
        v.a(1).x(1) := 999;
        wait for 1 ns;
        assert k.a(1).x(1) = 100;

        wait;
    end process;

end architecture;
