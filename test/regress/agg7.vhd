entity agg7 is
end entity;

architecture test of agg7 is
begin

    main: process is
        variable x : integer_vector(1 to 4);
        variable y, z : integer_vector(1 to 2);
    begin
        x := ( integer_vector'(1, 2), integer_vector'(3, 4) );
        assert x = (1, 2, 3, 4);
        y := (5, 6);
        z := (7, 8);
        x := ( y, z );
        assert x = (5, 6, 7, 8);
        x := ( 1 to 2 => z, 3 to 4 => integer_vector'(1, 2) );
        assert x = (7, 8, 1, 2);
        x := ( 4 downto 1 => x );
        assert x = (7, 8, 1, 2);
        x := ( y, 9, 8 );
        assert x = (5, 6, 9, 8);
        x := (9, 8, (1, 2 ));
        assert x = (9, 8, 1, 2);
        wait;
    end process;

end architecture;
