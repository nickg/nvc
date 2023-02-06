entity stateless1 is
end entity;

architecture test of stateless1 is
    signal x, y : integer;
begin

    p1: x <= y;

    p2: process is
        variable z : integer;
    begin
        z := x;
        wait for 1 ns;
        y <= z;
        wait;
    end process;

end architecture;
