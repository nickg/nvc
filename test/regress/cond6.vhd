entity cond6 is
end entity;

architecture test of cond6 is
begin

    p1: process is
        variable v : integer := 5;
    begin
        v := 1 when now = 5 ns else unaffected;
        assert v = 5;
        wait for 1 ns;
        v := 5 when now = 0 ns else unaffected when now = 2 ns else 66;
        assert v = 66;
        wait;
    end process;

end architecture;
