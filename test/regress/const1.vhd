entity const1 is
end entity;

architecture test of const1 is
    type int_vector is array (integer range <>) of integer;

    constant c : int_vector(1 to 5) := (1, 2, 3, 4, 5);
begin

    process is
        variable v : int_vector(1 to 2);
        variable i : integer;
    begin
        i := c(3);
        assert i = 3;
        v := c(1 to 2);
        assert v = (1, 2);
        v := c(3 to 4);
        assert v = (3, 4);
        wait;
    end process;

end architecture;
