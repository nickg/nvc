entity sub is
    port ( x : in integer;
           y : out integer );
end entity;

architecture test of sub is
begin

    with x select y <=
        5 when 1,
        7 when 2,
        99 when others;

end architecture;

-------------------------------------------------------------------------------

entity opencase is
end entity;

architecture test of opencase is
    signal x : integer;
begin

    uut: entity work.sub port map (x);

end architecture;
