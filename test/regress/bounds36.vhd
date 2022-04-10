entity bounds36 is
end entity;

architecture test of bounds36 is
begin

    main: process is
        variable x : integer_vector(4 downto 0);
        variable y : integer_vector(1 downto 0);
        variable z : integer_vector(2 downto 0);
        variable i0, i1 : integer;
    begin
        x := (1, 2, 3, 4, 5);
        i1 := 1;
        wait for 1 ns;
        (i1, y) := x(4 downto i1);                -- Error
        wait;
    end process;

end architecture;
