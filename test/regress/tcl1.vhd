entity tcl1 is
end entity;

architecture test of tcl1 is
    signal x : integer := 5;
begin

    process is
    begin
        x <= 6;
        wait for 1 ns;
        x <= 7;
        wait for 0 ns;
        wait;
    end process;

end architecture;
