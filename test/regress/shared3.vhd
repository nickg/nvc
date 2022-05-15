entity shared3 is
    shared variable x : integer;
end entity;

architecture test of shared3 is
begin

    p1: process is
    begin
        assert x = integer'left;
        x := 5;
        wait for 6 ns;
        assert x = 7;
        wait;
    end process;

    p2: process is
    begin
        wait for 1 ns;
        assert x = 5;
        x := 7;
        wait;
    end process;

end architecture;
