entity psl8 is
end entity;

architecture test of psl8 is
    signal clk : bit;

    -- psl default clock is clk'event and clk = '1';

    signal x, y : bit;
begin

    -- psl assert never ((x = '1') and y = '1');

    stim: process is
    begin
        wait for 1 ns;
        clk <= '1';
        wait for 1 ns;
        clk <= '0';
        x <= '1';
        wait for 1 ns;
        clk <= '1';
        wait for 1 ns;
        clk <= '0';
        y <= '1';
        wait for 1 ns;
        clk <= '1';
        -- Error
        wait;
    end process;

end architecture;
