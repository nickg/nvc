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

    p2: process is
    begin
        assert << constant ^.^.g(N).uut.k : integer >> = N;
        << signal ^.^.g(N).uut.x : integer >> <= force 1;
        wait for 1 ns;
        assert << signal ^.^.g(N).uut.x : integer >> = 1;
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity ename4 is
end entity;

architecture test of ename4 is
begin

    g: for i in 1 to 3 generate
        uut: entity work.bot generic map (N => i);
    end generate;

end architecture;
