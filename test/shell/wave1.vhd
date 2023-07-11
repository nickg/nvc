entity sub is
    port ( y : in bit );
end entity;

architecture test of sub is
begin
end architecture;

-------------------------------------------------------------------------------

entity wave1 is
end entity;

architecture test of wave1 is
    signal x : bit;
begin

    x <= '1' after 1 ns, '0' after 2 ns;

    u: entity work.sub port map ( x );

end architecture;
