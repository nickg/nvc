entity arith3 is
end entity;

architecture test of arith3 is
begin

    process is
        variable t : time;
        variable i : integer;
    begin
        t := 120 ns;
        i := sec / t;
        report integer'image(i);
        assert i = 8333333;
        wait;
    end process;

end architecture;
