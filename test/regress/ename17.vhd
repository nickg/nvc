entity bot is
end entity;

architecture test of bot is
begin

    p2: process is
    begin
        assert <<signal ^.^.^.^.x : bit>> = '0';  -- Error
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity ename17 is
end entity;

architecture test of ename17 is
begin

    uut: entity work.bot;

end architecture;
