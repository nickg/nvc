entity wait12 is
end entity;

architecture test of wait12 is
    signal x, y : integer;
begin

    a: process is
    begin
        wait for 1 ns;
        x <= 1;
        wait for 0 ns;
        x <= 2;
        wait for 0 ns;
        x <= 3;
        wait for 1 ns;
        x <= 4;
        wait on y;
        report "wake up other process";
        assert y = 2;
        wait;
    end process;

    b: postponed process is
    begin
        wait on x;
        report "wake up postponed process 1";
        assert x = 3;
        wait on x;
        report "wake up postponed process 2";
        y <= 2 after 1 ns;
        wait;
    end process;

end architecture;
