entity sub is
    generic (g : bit_vector);
    port (o : out integer);
end entity;

architecture test of sub is
begin

    gg: case g generate
        when "101" =>
            o <= 1;
        when "100" =>
            o <= 2;
        when "001" =>
            o <= 3;
        when others =>
            assert false report "invalid G" severity failure;
    end generate;

end architecture;

-------------------------------------------------------------------------------

entity case15 is
end entity;

architecture test of case15 is
    signal o1, o2, o3 : integer;
begin

    u1: entity work.sub
        generic map ( "101" )
        port map ( o1 );

    u2: entity work.sub
        generic map ( "100" )
        port map ( o2 );

    u3: entity work.sub
        generic map ( "001" )
        port map ( o3 );

    check: process is
    begin
        wait for 1 ns;
        assert o1 = 1;
        assert o2 = 2;
        assert o3 = 3;
        wait;
    end process;

end architecture;
