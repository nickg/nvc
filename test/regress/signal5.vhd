entity signal5 is
end entity;

architecture test of signal5 is
    signal x, y : integer;
begin

    update_y: y <= x + 4;

    stim: process is
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
