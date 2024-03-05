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
        alias a1 is <<signal uut.z : integer>>;  -- Error (once)
        alias a2 is <<signal uut.x : bit>>;  -- Error
    begin
        assert <<signal uut.x : integer>> = 0;  -- OK
        assert <<variable uut.x : integer>> = 0;  -- Error
        assert <<signal bot.x : integer>> = 0;  -- Error
        assert <<signal .ename1.uut.x : integer>> = 0;  -- OK
        assert <<signal .ename1.uut.x : bit>> = '0';  -- Error
        assert <<signal uut(1).x : integer>> = 0;  -- Error
        assert <<signal .foo.x : integer>> = 0;  -- Error
        assert <<signal .ename1.uut : bit>> = '0';  -- Error
        assert <<signal uut.x.y : integer>> = 0;  -- Error
        assert a1 = 1;
        assert a1 = 2;
        assert a2 = '1';
        wait;
    end process;

end architecture;
