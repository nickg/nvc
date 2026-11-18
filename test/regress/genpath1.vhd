entity genpath1_leaf is
    generic ( W : integer := 1; TAG : string := "d"; EXPECT : integer );
end entity;

architecture test of genpath1_leaf is
begin
    check: process is
    begin
        assert W = EXPECT
            report genpath1_leaf'path_name & " W=" & integer'image(W)
            & " expected " & integer'image(EXPECT) severity failure;
        assert TAG = "x" report genpath1_leaf'path_name & " TAG=" & TAG
            severity failure;
        wait;
    end process;
end architecture;

-------------------------------------------------------------------------------

entity genpath1_mid is
    generic ( W : integer := 2 );
end entity;

architecture test of genpath1_mid is
    component genpath1_leaf is
        generic ( W : integer := 1; TAG : string := "d"; EXPECT : integer );
    end component;
begin
    -- Run with -gm.gen(1).g.W=9 -gm.blk.b.W=8 -gm.comp.W=7 -gx.W=6
    -- -g*.TAG=x
    gen: for i in 1 to 2 generate
        g: entity work.genpath1_leaf
            generic map ( EXPECT => 9 - 8 * (i - 1) );   -- 9 then 1
    end generate;

    blk: block is
    begin
        b: entity work.genpath1_leaf generic map ( EXPECT => 8 );
    end block;

    comp: genpath1_leaf generic map ( EXPECT => 7 );

    direct: entity work.genpath1_leaf
        generic map ( W => W * 10, EXPECT => 20 );
end architecture;

-------------------------------------------------------------------------------

entity genpath1 is
end entity;

architecture test of genpath1 is
begin
    m: entity work.genpath1_mid;

    -- The name without the generate label applies to every copy
    top_gen: for k in 0 to 1 generate
        x: entity work.genpath1_leaf generic map ( EXPECT => 6 );
    end generate;
end architecture;
