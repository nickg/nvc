entity wait11 is
end entity;

architecture test of wait11 is
begin

    process is
    begin
        wait for 0.1 ns;
        assert now = 100 ps;
        wait for 0.5 ns;
        assert now = 600 ps;
        wait for 1 ns / 10.0;
        assert now = 700 ps;
        wait for 10 ps * 10.0;
        assert now = 800 ps;
        wait;
    end process;

end architecture;
