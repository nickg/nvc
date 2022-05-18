entity func19 is
end entity;

architecture test of func19 is

    function maybe_not_return (x : integer) return integer is
    begin
        if x > 0 then
            return x * 2;
        end if;
    end function;

    signal x, y : integer := 0;

begin

    p1: y <= maybe_not_return(x);

    p2: process is
    begin
        x <= 55;
        wait for 1 ns;
        assert y = 110;
        x <= -1;
        wait for 1 ns;
        assert false report "should not reach here: " & integer'image(y);
        wait;
    end process;

end architecture;
