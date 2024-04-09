entity sub is
    port ( o : out natural );
end entity;

architecture test of sub is
    signal s : natural;
begin
    o <= s + 1 after 1 ns;
end architecture;

-------------------------------------------------------------------------------

entity issue877 is
end entity;

architecture test of issue877 is
    signal x : natural;
begin

    uut: entity work.sub
        port map ( x );

    update: process (all) is
        alias s is << signal uut.s : natural >>;
    begin
        if x = 10 then
            s <= release;
        else
            s <= force x;
        end if;
    end process;

    check: process is
    begin
        wait for 1 ns;
        assert x = 1;
        wait for 1 ns;
        assert x = 2;
        wait for 8 ns;
        assert x = 10;
        wait for 1 ns;
        assert x = 1;
        std.env.finish;
        wait;
    end process;

end architecture;
