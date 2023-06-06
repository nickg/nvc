entity real5 is
end entity;

architecture test of real5 is
    type real_array is array (natural range <>) of real;
begin

    p1: process is
        variable v : real_array(1 to 3);
    begin
        v := (1.0, 2.0, 3.0);
        assert v < (2.0, 3.0, 4.0);
        assert v <= v;
        assert v > (0 => 1.0);
        assert v >= v;
        assert maximum(v) = 3.0;
        assert minimum(v) = 1.0;
        wait;
    end process;

end architecture;
