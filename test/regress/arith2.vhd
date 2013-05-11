entity arith2 is
end entity;

architecture test of arith2 is
begin

    process is
        variable x, y : integer;
    begin
        x := 5;
        y := 0;
        wait for 1 ns;
        x := x / y;
        wait;
    end process;

end architecture;
