entity slice1 is
end entity;

architecture test of slice1 is
    type int_vector is array (integer range <>) of integer;
    signal x : int_vector(0 to 3);
begin

    p1: process is
        variable u : int_vector(5 downto 2);
        variable v : int_vector(0 to 3);
    begin
        v := ( 1, 2, 3, 4 );
        v(1 to 2) := ( 6, 7 );
        assert v(2 to 3) = ( 7, 4 );
        wait for 1 ns;

        x <= ( 1, 2, 3, 4 );
        x(1 to 2) <= ( 6, 7 );
        assert x(2 to 3) = ( 7, 4 );
        wait for 1 ns;

        u := ( 1, 2, 3, 4);
        u(4 downto 3) := ( 6, 7 );
        assert u(3 downto 2) = ( 7, 4 );

        wait;
    end process;

end architecture;
