entity issue1150 is
end entity;

architecture test of issue1150 is
    signal s : integer;
    signal clk : bit;

    impure function func (x : integer) return boolean is
    begin
        return x > 10;
    end function;

    -- psl default clock is clk'event;
begin

    -- OR has control flow
    -- psl assert always s = 5 or func(s);

    process is
    begin
        s <= 5;
        clk <= not clk;
        s <= 11;
        clk <= not clk;
        wait;
    end process;

end architecture;
