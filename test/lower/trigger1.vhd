entity trigger1 is
end entity;

architecture test of trigger1 is
    function rising (signal x : bit) return boolean is
    begin
        return x'event and x = '1';
    end function;

    function bad (x : bit) return boolean is
    begin
        return x = '1';
    end function;

    signal clk, x, y, z, rstn : bit;
begin

    p1: process (clk) is
    begin
        if rising(clk) then
            x <= not x;
        end if;
    end process;

    p2: process (clk) is
    begin
        if bad(clk) then
            x <= not x;
        end if;
    end process;

    p3: process (rstn, clk) is
    begin
        if rstn = '0' then
            y <= '0';
        elsif rising(clk) then
            y <= not y;
        end if;
    end process;

    p4: process (clk) is
    begin
        if clk'event and clk = '1' then
            z <= not z;
        end if;
    end process;
end architecture;
