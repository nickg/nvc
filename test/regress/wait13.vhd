entity wait13 is
end entity;

architecture test of wait13 is

    procedure wait_for (
        signal s : in bit;
        value    : in bit;
        timeout  : in delay_length ) is
    begin
        if s /= value then
            my_wait: wait on s until s = value for timeout;
            assert s = value
                report "timeout waiting for " & s'path_name;
        end if;
    end procedure;

    signal x : bit;

begin

    wait_for_p: process is
    begin
        wait_for(x, '1', 5 ns);
        wait_for(x, '0', 10 us);
        wait until x = '0' for 10 ns;
        assert now = 12 ns;
        wait;
    end process;

    stim_p: process is
    begin
        wait for 1 ns;
        x <= '1';
        wait for 1 ns;
        x <= '0';
        wait for 4 ns;
        x <= '1';
        wait;
    end process;

end architecture;
