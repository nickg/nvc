entity agg6 is
end entity;

architecture test of agg6 is

    type int_vec is array (natural range <>) of integer;
    type int_vec4x2 is array (1 to 4) of int_vec(1 to 2);

begin

    process is
        variable a : int_vec4x2;
        variable b : int_vec(1 to 8);
    begin
        b := (1, 2, 3, 4, 5, 6, 7, 8);
        a := int_vec4x2'(b(1 to 2), b(3 to 4), b(5 to 6), b(7 to 8));
        assert a(1) = (1, 2);
        assert a(2) = (3, 4);
        assert a(3) = (5, 6);
        assert a(4) = (7, 8);
        wait;
    end process;

end architecture;
