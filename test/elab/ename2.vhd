entity bot is
end entity;

architecture test of bot is
    constant k : integer := 5;
    signal x, y : natural;
begin

    p1: process (y) is
    begin
        x <= y + 5;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity ename2 is
end entity;

architecture test of ename2 is
    constant c : integer := <<constant .ename2.uut.k : integer>>;  -- Error
begin

    uut: entity work.bot;

end architecture;
