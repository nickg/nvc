entity array3 is
end entity;

architecture test of array3 is
    type matrix2x4 is array (1 to 2, 1 to 4) of integer;
    signal m : matrix2x4;
begin

    process is
    begin
        assert m(2, 2) = integer'left;
        m(2, 2) <= 5;
        wait for 1 ns;
        assert m(2, 2) = 5;
        m(2, 3) <= m(2, 2);
        wait for 1 ns;
        assert m(2, 3) = 5;

        m <= ( (1, 2, 3, 4),
               (5, 6, 7, 8) );
        wait for 1 ns;
        assert m(2, 4) = 8;

        wait;
    end process;

end architecture;
