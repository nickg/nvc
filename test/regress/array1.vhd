entity array1 is
end entity;

architecture test of array1 is
    type matrix_t is array (integer range <>, integer range <>) of integer;

    constant c : matrix_t(0 to 1, 0 to 1) := (
        ( 1, 2 ),
        ( 3, 4 ) );
begin

    process is
        variable m : matrix_t(1 to 3, 1 to 3) := (
            ( 1, 2, 3 ),
            ( 4, 5, 6 ),
            ( 7, 8, 9 ) );
    begin
        report integer'image(m(1, 3));
        report integer'image(m(2, 2));
        assert m(2, 2) = 5;
        assert m(3, 1) = 7;
        report integer'image(c(1, 0));
        assert c(1, 0) = 3;

        assert c = ( (1, 2), (3, 4) );
        assert c /= ( (1, 2), (3, 5) );

        wait;
    end process;

end architecture;
