entity const3 is
end entity;

architecture test of const3 is
    type real_vector is array (natural range <>) of real;

    function get_reals return real_vector is
    begin
        return (1.0, 0.0, 1.0, 0.0, 1.0);
    end function;
begin

    p1: process is
        constant c1 : real_vector := get_reals;
        constant c2 : real_vector(1 to c1'length) := (0.0, 1.0, 0.0, 1.0, 1.0);
    begin
        wait for 5 ns;
        assert false;
    end process;

end architecture;
