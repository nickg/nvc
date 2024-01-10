entity issue825 is
end entity;

architecture test of issue825 is
    signal s : bit_vector(1 to 3);
    signal i : integer range 1 to 3;
begin

    p1: s <= (1 to i => '1') & (i + 1 to 3 => '0');

    p2: process is
    begin
        assert s = "000";
        wait for 0 ns;
        assert s = "100";
        i <= 2;
        wait for 0 ns;
        wait for 0 ns;
        assert s = "110";
        i <= 3;
        wait for 0 ns;
        wait for 0 ns;
        assert s = "111";
        wait;
    end process;

end architecture;
