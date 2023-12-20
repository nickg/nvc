entity \foo\ is
    port ( x : out integer );
end entity;

architecture \test\ of \foo\ is
begin
    x <= 1;
end architecture;

-------------------------------------------------------------------------------

entity \Foo\ is
    port ( x : out integer );
end entity;

architecture \test\ of \Foo\ is
begin
    x <= 2;
end architecture;

-------------------------------------------------------------------------------

entity issue816 is
end entity;

architecture test of issue816 is
    signal a, b : integer;
begin

    u1: entity work.\foo\ port map ( a );

    u2: entity work.\Foo\ port map ( b );

    check: process is
    begin
        wait for 1 ns;

        assert a = 1 severity failure;
        assert b = 2 severity failure;

        wait for 1 ns;

        -- Make this test fail so that run_regr does analysis/elaboration and
        -- execution in two separate processes, causing the library units to be
        -- re-read from disk
        report "passed" severity failure;

        wait;
    end process;

end architecture;
