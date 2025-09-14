entity wait29 is
end entity;

architecture test of wait29 is
    signal s : natural;
    signal done : boolean;
begin

    p1: process is
    begin
        wait until s = 5 for 10 ns;
        assert s = 2;
        assert now = 10 ns;
        done <= true;
        wait;
    end process;

    p2: process is
    begin
        wait for 1 ns;
        s <= 1;
        wait for 1 ns;
        s <= 2;
        wait for 10 ns;
        assert done;
        wait;
    end process;

end architecture;
