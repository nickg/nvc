entity pending1 is
end entity;

architecture test of pending1 is
    signal state : integer := 0;
    signal x     : integer := 0;
begin

    wakeup: process is
    begin
        wait until x = 1;
        state <= 1;
        wait until x = 5;
        state <= 2;
        wait until x > 10;
        state <= 3;
        wait;
    end process;

    stim: process is
    begin
        x <= -1;
        wait for 1 ns;
        x <= 6;
        wait for 1 ns;
        x <= 1;
        wait for 1 ns;
        x <= 0;
        wait for 1 ns;
        x <= 5;
        wait for 1 ns;
        x <= 50;
        wait for 1 ns;
        wait;
    end process;

end architecture;
