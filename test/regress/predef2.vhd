entity predef2 is
end entity;

architecture test of predef2 is
    type int_vector is array (natural range <>) of integer;

    type bit_vector is array (natural range <>) of bit;
begin

    main: process is
        variable p, q : int_vector(1 to 3);
        variable bv1  : bit_vector(3 downto 0);
        variable iv1  : int_vector(3 downto 0);
        variable b1   : bit;
        variable int1 : integer;
    begin
        p := (1, 2, 3);
        q := (4, 5, 6);
        wait for 1 ns;
        assert p < q;
        assert minimum(p, q) = (1, 2, 3);
        assert maximum(p, q) = (4, 5, 6);

        bv1 := "0100";
        b1  := maximum (bv1);
        assert b1 = '1' report to_string(b1);

        iv1  := (5, 4, 30, 2);
        int1 := minimum(iv1);
        assert int1 = 2 report to_string(int1);

        wait;
    end process;

end architecture;
