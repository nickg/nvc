entity signal20 is
end entity;

architecture test of signal20 is
    signal x : integer_vector(4 downto 0);
    signal y : integer_vector(1 downto 0);
    signal z : integer_vector(2 downto 0);
    signal i0, i1 : integer;
begin

    main: process is
    begin
        x <= (1, 2, 3, 4, 5);
        wait for 1 ns;
        (y, z) <= x;
        wait for 1 ns;
        assert y = (1, 2);
        assert z = (3, 4, 5);
        wait for 1 ns;
        (i0, z, i1) <= x;
        wait for 1 ns;
        assert i0 = 1;
        assert z = (2, 3, 4);
        assert i1 = 5;
        wait;
    end process;

end architecture;
