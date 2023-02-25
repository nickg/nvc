entity wait26 is
end entity;

architecture test of wait26 is
begin

    p1: process is
        variable delay : time;
    begin
        delay := 5 ns;
        wait for delay;
        assert now = 5 ns;
        delay := -1 hr;
        wait for 1 ns;
        wait for delay;                 -- Error
        wait;
    end process;

end architecture;
