entity arith5 is
end entity;

architecture test of arith5 is
begin

    p1: process is
        variable x, y : integer;
    begin
        x := 5;
        y := 2;
        wait for 1 ns;
        assert x mod y = 1;
        y := 0;
        wait for 1 ns;
        assert x mod y = 0;             -- Error
        wait;
    end process;

end architecture;
