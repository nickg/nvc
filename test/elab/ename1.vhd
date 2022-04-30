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

entity ename1 is
end entity;

architecture test of ename1 is
begin

    uut: entity work.bot;

    p2: process is
    begin
        assert <<signal uut.x : integer>> = 0;  -- OK
        assert <<variable uut.x : integer>> = 0;  -- Error
        assert <<signal bot.x : integer>> = 0;  -- Error
        assert <<signal .ename1.uut.x : integer>> = 0;  -- OK
        assert <<signal .ename1.uut.x : bit>> = '0';  -- Error
        assert <<signal ^.x : bit>> = '0';  -- Error
        wait;
    end process;

end architecture;
