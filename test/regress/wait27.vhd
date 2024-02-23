entity wait27 is
end entity;

architecture test of wait27 is
    function rising (signal x : bit) return boolean is
    begin
        return x'event and x = '1';
    end function;

    function falling (signal x : bit) return boolean is
    begin
        return x'event and x = '0';
    end function;

    signal clk, rstn : bit := '0';
    signal count1, count2 : natural;
    signal running : boolean := true;
begin

    clkgen: clk <= not clk after 5 ns when running;

    p1: process (rstn, clk) is
    begin
        if rstn = '0' then
            count1 <= 0;
        elsif rising(clk) then
            count1 <= count1 + 1;
        end if;
    end process;

    p2: process (rstn, clk) is
    begin
        if rstn = '0' then
            count2 <= 0;
        elsif clk'event and clk = '0' then
            count2 <= count1 + 1;
        end if;
    end process;

    p3: process (clk) is
    begin
        if rising(clk) then
            assert now > 0 ns;
        end if;
    end process;

    check: process is
    begin
        wait until falling(clk);
        rstn <= '1';
        wait for 50 ns;
        assert count1 = 5;
        assert count2 = 5;
        running <= false;
        wait for 20 ns;
        assert count1 = 6;
        assert count2 = 6;
        rstn <= '0';
        wait for 0 ns;
        wait for 0 ns;
        assert count1 = 0;
        assert count2 = 0;
        wait;
    end process;

end architecture;
