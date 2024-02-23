entity trigger1 is
end entity;

architecture test of trigger1 is
    function rising (signal x : bit) return boolean is
    begin
        return x'event and x = '1';
    end function;

    signal clk, x : bit;
begin

    p1: process (clk) is
    begin
        if rising(clk) then
            x <= not x;
        end if;
    end process;

end architecture;
