entity signal5 is
end entity;

architecture test of signal5 is
    signal x, y : integer;
begin

    y <= x + 4;

    process is
    begin
        x <= 1;
        wait for 1 ns;
        assert y = 5;
        x <= 10;
        wait on y;
        assert y = 14;
        wait;
    end process;
    
end architecture;

