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
    signal b : boolean;
begin

    x <= '1' after 1 ns, '0' after 2 ns;

    b <= true after 1 ns;

    u: entity work.sub port map ( x );

end architecture;
