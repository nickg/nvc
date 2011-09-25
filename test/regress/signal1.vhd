entity signal1 is
end entity;

architecture test of signal1 is
    signal x : integer := 5;
begin

    process is
    begin
        assert x = 5 report "initial value not 5";
        x <= 6;
        assert x = 5 report "x changed after nb assign";
        wait for 1 ns;
        assert x = 6 report "x not updated";
        x <= 7;
        wait for 0 ns;
        assert x = 7 report "x not updated after delta cycle";
        wait;
    end process;

end architecture;
