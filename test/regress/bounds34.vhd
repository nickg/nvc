entity bounds34 is
end entity;

architecture test of bounds34 is
begin

    main: process is
        variable x : integer_vector(1 to 4);
        variable y, z : integer_vector(1 to 2);
    begin
        x := ( integer_vector'(1, 2), integer_vector'(3, 4) );
        assert x = (1, 2, 3, 4);
        y := (5, 6);
        z := (7, 8);
        x := ( y, 0, integer_vector'(1, 2, 3) );  -- Error
        assert x = (5, 6, 7, 8);
        wait;
    end process;

end architecture;
