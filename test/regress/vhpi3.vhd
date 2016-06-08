entity vhpi3 is
end entity;

architecture test of vhpi3 is
    type weight is range -100 to 4000
        units
            g;
            kg = 1000 g;
        end units;

    signal x : weight := 2 g;
begin
end architecture;
