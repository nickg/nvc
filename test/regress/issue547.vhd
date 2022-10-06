entity sub is
    signal x : bit;
end entity;

architecture test of sub is
begin

    x <= '1';

    process is
    begin
        assert x = '0';
        wait for 0 ns;
        assert x = '1';
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity issue547 is
    signal s : natural;
end entity;

architecture test of issue547 is
begin

    u: entity work.sub;

    p1: process is
    begin
        assert s = 0;
        s <= 1;
        wait for 1 ns;
        assert s = 1;
        wait;
    end process;

end architecture;
