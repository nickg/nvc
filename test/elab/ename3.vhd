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

entity ename3 is
end entity;

architecture test of ename3 is
begin

    uut: entity work.bot;

    p2: process is
    begin
        assert <<signal ^.x : bit>> = '0';  -- Error
        assert <<signal ^.^.^.^.x : bit>> = '0';  -- Error
        wait;
    end process;

end architecture;
