entity sub is
    port ( ii : in bit; oo : out bit );
end entity;

architecture test of sub is
begin
    oo <= ii after 1 ps;
end architecture;

-------------------------------------------------------------------------------

entity wave9 is
end entity;

architecture test of wave9 is
    component sub is
        port ( i : in bit; o : out bit );
    end component;

    for u : sub use entity work.sub
        port map ( ii => i, oo => o );

    signal x, y : bit;
begin

    u: sub port map ( x, y );

    x <= '1' after 1 ns, '0' after 2 ns;

end architecture;
