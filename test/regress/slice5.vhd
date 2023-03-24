entity slice5 is
end entity;

architecture test of slice5 is
    type int_vec is array (natural range <>) of integer;
begin

    p1: process is
        variable v : int_vec(1 to 8) := (1, 2, 3, 4, 5, 6, 7, 8);
    begin
        v(1 to 2) := v(2 to 3);
        assert v(1 to 2) = (2, 3);
        v(4 to 5) := v(3 to 4);
        assert v(4 to 5) = (3, 4);
        wait for 1 ns;
        v(1 to 7) := v(2 to 8);
        assert v = (3, 3, 3, 4, 6, 7, 8, 8);
        v(2 to 8) := v(1 to 7);
        assert v = (3, 3, 3, 3, 4, 6, 7, 8);

        wait;
    end process;

end architecture;
