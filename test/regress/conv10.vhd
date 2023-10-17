entity conv10 is
end entity;

architecture test of conv10 is
begin

    p1: process is
        variable i : integer_vector(1 to 3);
        variable r : real_vector(1 to 3);
    begin
        i := (1, 2, 3);
        r := real_vector(i);
        assert r = (1.0, 2.0, 3.0) severity failure;
        r := (4.2, 5.9, 7.1);
        i := integer_vector(r);
        assert i = (4, 6, 7) severity failure;
        r := (1.0, real'left, 5.3);
        i := integer_vector(r);     -- Range error
        wait;
    end process;

end architecture;
