entity real4 is
end entity;

architecture test of real4 is
begin

    p1: process is
        variable x, y : real;
    begin
        x := 1.0;
        y := 0.0;
        wait for 1 ns;
        x := x / y;             -- Error
        wait;
    end process;

end architecture;
