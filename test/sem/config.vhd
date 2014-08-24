entity sub1 is
end entity;

architecture a of sub1 is
begin
end architecture;

-------------------------------------------------------------------------------

entity sub2 is
end entity;

architecture a of sub2 is
begin
end architecture;

-------------------------------------------------------------------------------

entity top is
end entity;

architecture a of top is

    component comp is
    end component;

begin

    c1: component comp;

end architecture;

-------------------------------------------------------------------------------

configuration conf of top is
    for a
        for c1 : comp
            use entity work.sub2(a);
        end for;
    end for;
end configuration;
