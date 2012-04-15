entity sub is
    port (
        x : out integer );
end entity;

architecture one of sub is
begin
    x <= 1;
end architecture;

architecture two of sub is
begin
    x <= 2;
end architecture;

-------------------------------------------------------------------------------

entity elab5 is
end entity;

architecture test of elab5 is
    signal x1, x2, x3 : integer;
begin

    sub1: entity work.sub(one)
        port map ( x1 );

    sub2: entity work.sub(two)
        port map ( x2 );

    sub3: entity work.sub               -- Should select `two'
        port map ( x3 );

    process is
    begin
        wait for 1 ns;
        assert x1 = 1;
        assert x2 = 2;
        assert x3 = 2;
        wait;
    end process;

end architecture;

    
