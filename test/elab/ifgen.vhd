entity sub is
    generic (
        foo : boolean := true );
    port (
        x : out integer );
end entity;

architecture test of sub is
begin

    g: if foo = true generate
        x <= 5;
    end generate;

end architecture;

-------------------------------------------------------------------------------

entity ifgen is
end entity;

architecture test of ifgen is
    signal x : integer;
begin

    sub_i: entity work.sub
        port map ( x );

end architecture;
