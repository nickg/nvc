entity genpath2_leaf is
    generic ( W : integer := 1; EXPECT : integer );
end entity;

architecture test of genpath2_leaf is
begin
    check: process is
    begin
        assert W = EXPECT
            report genpath2_leaf'path_name & " W=" & integer'image(W)
            & " expected " & integer'image(EXPECT) severity failure;
        wait;
    end process;
end architecture;

-------------------------------------------------------------------------------

entity genpath2 is
    generic ( W : integer := 2 );
end entity;

-- Elaborated with --generic-default=*.W=5 --generic-default=c.u.W=4
architecture test of genpath2 is
begin
    a: entity work.genpath2_leaf generic map ( EXPECT => 5 );
    b: entity work.genpath2_leaf generic map ( W => 3, EXPECT => 3 );
    c: block is
    begin
        u: entity work.genpath2_leaf generic map ( W => open, EXPECT => 4 );
    end block;
    t: entity work.genpath2_leaf generic map ( W => W + 1, EXPECT => 6 );
end architecture;
