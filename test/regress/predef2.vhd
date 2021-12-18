entity predef2 is
end entity;

architecture test of predef2 is
    type int_vector is array (natural range <>) of integer;
begin

    main: process is
        variable p, q : int_vector(1 to 3);
    begin
        p := (1, 2, 3);
        q := (4, 5, 6);
        wait for 1 ns;
        assert p < q;
        assert minimum(p, q) = (1, 2, 3);
        assert maximum(p, q) = (4, 5, 6);

        wait;
    end process;

end architecture;
