entity cover14 is
end cover14;

architecture test of cover14 is

    signal x   : integer := 10;
    signal y   : integer := 0;

    signal res : boolean;
    signal res_2 : boolean;

begin

    res <= (y /= 0) and ((x / y) = 1);

    -- Should not execute div by zero if the first condition is met
    -- if not, then it is not div by zero
    res_2 <= (y = 0) or ((x / y) = 1);

    process
    begin
        x <= 10;
        wait for 1 ns;
        y <= 5;
        wait for 1 ns;
        wait;
    end process;

end architecture;

