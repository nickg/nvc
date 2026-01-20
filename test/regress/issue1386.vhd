entity sub_impl is
    port ( o : out integer );
end entity;

architecture test of sub_impl is
begin
    assign: o <= 42;
end architecture;

-------------------------------------------------------------------------------

entity sub is
    port ( o : out integer );
end entity;

architecture test of sub is
begin
    impl: entity work.sub_impl
        port map ( o );
end architecture;

-------------------------------------------------------------------------------

entity issue1386 is
end entity;

architecture test of issue1386 is
    component comp is
        port ( o : out integer );
    end component;

    for all : comp use entity work.sub;

    signal s : integer;
begin

    u: component comp port map ( s );

    check: process is
    begin
        wait for 1 ns;
        assert s = 42;
        wait;
    end process;

end architecture;
