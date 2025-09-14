entity wait30 is
end entity;

architecture test of wait30 is
    signal x : natural;
begin

    process is
    begin
        x <= 1;
        x <= 5 after 1 ns;
        wait for 1 ns;
        x <= 6;
        wait for 0 ns;
        x <= 7;
        wait;
    end process;

    postponed process is
    begin
        assert x = 0;
        wait for 1 ns;
        assert x = 7;
        wait;
    end process;

end architecture;
