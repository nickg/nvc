entity bounds37 is
end entity;

architecture test of bounds37 is
    signal x : integer_vector(4 downto 0);
    signal y : integer_vector(1 downto 0);
    signal z : integer_vector(2 downto 0);
    signal i0, i1 : integer;
begin

    main: process is
    begin
        x <= (1, 2, 3, 4, 5);
        wait for 1 ns;
        i1 <= 1;
        wait for 1 ns;
        (i1, y) <= x(4 downto i1);                -- Error
        wait;
    end process;

end architecture;
