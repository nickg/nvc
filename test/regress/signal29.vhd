entity signal29 is
end entity;

architecture test of signal29 is
    signal x : integer;
begin

    p1: process is
        variable delay : time;
    begin
        delay := -10 ps;
        wait for 1 ns;
        x <= 1, 2 after delay;
        wait;
    end process;

end architecture;
