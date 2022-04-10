entity assign7 is
end entity;

architecture test of assign7 is
begin

    main: process is
        variable x : integer_vector(4 downto 0);
        variable y : integer_vector(1 downto 0);
        variable z : integer_vector(2 downto 0);
        variable i0, i1 : integer;
    begin
        x := (1, 2, 3, 4, 5);
        (y, z) := x;
        assert y = (1, 2);
        assert z = (3, 4, 5);
        (i0, z, i1) := x;
        assert i0 = 1;
        assert z = (2, 3, 4);
        assert i1 = 5;
        wait;
    end process;

end architecture;
