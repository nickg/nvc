entity bot is
end entity;

architecture test of bot is
    signal x, y : natural;
begin

    p1: process (y) is
    begin
        x <= y + 5;
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
        assert <<signal uut.x : natural>> = 0;
        wait for 1 ns;
        <<signal uut.y : natural>> <= force 5;
        wait for 0 ns;
        assert <<signal uut.x : natural>> = 5;
        wait for 0 ns;
        assert <<signal .ename1.uut.x : natural>> = 10;
        <<signal uut.y : natural>> <= release;
        wait for 1 ns;
        assert <<signal .ename1.uut.x : natural>> = 5;
        wait for 10 ns;
        assert <<signal .ename1.uut.x : natural>> = 25;
        wait;
    end process;

    p3: process is
        alias y is <<signal uut.y : natural>>;
    begin
        y <= 0;
        wait for 10 ns;
        y <= 20;
        wait;
    end process;

end architecture;
