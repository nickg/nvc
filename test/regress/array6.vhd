entity array6 is
end entity;

architecture test of array6 is

    type int_vec2 is array (natural range <>) of integer_vector;

    constant a : int_vec2(1 to 3)(1 to 2) := ( (1, 2), (3, 4), (5, 6) );

begin

    main: process is
        variable v : a'subtype := a;
    begin
        assert a(1)(1) = 1;
        assert a(3)(2) = 6;
        wait for 1 ns;
        assert v(1)(1) = 1;
        assert v(3)(2) = 6;
        assert v = a;
        v(1)(2) := 99;
        assert v /= a;
        assert v(1) = (1, 99);
        v(2) := (55, 666);
        assert v(2)(2) = 666;
        assert v(2 to 3) = ((55, 666), (5, 6));
        wait;
    end process;

end architecture;
