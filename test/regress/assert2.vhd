entity assert2 is
end entity;

architecture test of assert2 is
    signal x : integer;
begin

    process is
    begin
        x <= 5;
        wait for 1 ns;
        x <= 12;
        wait for 1 ns;
        wait;
    end process;

    assert x < 10 report "x >= 10" severity warning;

end architecture;
