entity bot is
end entity;

architecture test of bot is
    signal x, y : natural;
begin

    p1: process is
    begin
        wait for 5 ns;
        x <= 5;
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity ename9 is
end entity;

architecture test of ename9 is
begin

    uut: entity work.bot;

    p2: process is
        alias a2 is <<signal uut.x : bit>>;  -- Error
    begin
        assert a2 = '1';
        wait;
    end process;

end architecture;
