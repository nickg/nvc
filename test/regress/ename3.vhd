entity bot is
    generic ( N : integer );
end entity;

architecture test of bot is
    constant k : integer := N;
    signal x, y : natural;
begin

    p1: process (y) is
    begin
        x <= y + 5;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity ename3 is
end entity;

architecture test of ename3 is
begin

    g: for i in 1 to 3 generate
        uut: entity work.bot generic map (N => i);
    end generate;

    p1: process is
    begin
        assert << constant g(1).uut.k : integer >> = 1;
        assert << constant g(2).uut.k : integer >> = 2;
        assert << constant g(3).uut.k : integer >> = 3;
        << signal g(1).uut.x : integer >> <= force 1;
        wait for 1 ns;
        assert << signal g(1).uut.x : integer >> = 1;
        wait;
    end process;

end architecture;
