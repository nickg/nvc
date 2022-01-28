entity wait16 is
end entity;

architecture test of wait16 is
    type int_vec is array (natural range <>) of integer;

    function get_4_ints(a, b, c, d : integer) return int_vec is
    begin
        return (a, b, c, d);
    end function;

begin

    p1: process is
        constant x : int_vec := (1, 2, 3, 4, 5);
        variable y : int_vec(1 to x'length) := x;
    begin
        wait for 5 ns;
        assert y = (1, 2, 3, 4, 5);
        wait;
    end process;

    p2: process is
        constant x : int_vec := (6, 7, 8, 9);
        variable y : int_vec(1 to x'length) := x;
    begin
        wait for 5 ns;
        assert get_4_ints(1, 2, 3, 4) = (1, 2, 3, 4);  -- Would overwrite y
        assert y = (6, 7, 8, 9);
        wait;
    end process;

end architecture;
