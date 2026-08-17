entity sub is
end entity;

architecture Test of sub is
begin
end architecture;

-------------------------------------------------------------------------------

entity top is
end entity;

architecture Test of top is
    component comp is
    end component;
begin
    U1: component comp;

    B1: block is
    begin
        U2: component comp;
    end block;
end architecture;

-------------------------------------------------------------------------------

configuration casecmp1 of top is
    for test
        for u1 : comp
            use entity work.sub(test);
        end for;

        for b1
            for u2 : comp
                use entity work.sub(test);
            end for;
        end for;
    end for;
end configuration;
