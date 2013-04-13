entity sub2 is
    generic (
        VALUE : integer );
    port (
        x : out integer );
end entity;

architecture test of sub2 is
begin
    x <= VALUE;
end architecture;

-------------------------------------------------------------------------------

entity sub1 is
    generic (
        ENABLE : boolean );
    port (
        y : out integer );
end entity;

architecture test of sub1 is
begin

    value7_g: if ENABLE generate

        sub: entity work.sub2
            generic map ( VALUE => 7 )
            port map ( x => y );

    end generate;

    value5_g: if not ENABLE generate

        sub: entity work.sub2
            generic map ( VALUE => 5 )
            port map ( x => y );

    end generate;

end architecture;

-------------------------------------------------------------------------------

entity elab7 is
end entity;

architecture test of elab7 is
    signal a, b : integer;
begin

    sa: entity work.sub1
        generic map ( ENABLE => true )
        port map ( y => a );

    sb: entity work.sub1
        generic map ( ENABLE => false )
        port map ( y => b );

    process is
    begin
        wait for 1 ns;
        assert a = 7;
        assert b = 5;
        wait;
    end process;

end architecture;
