entity casegen is
end entity;

architecture test of casegen is
    constant k : integer := 5;
    signal s : integer;
begin

    g1: case k generate
        when 1 => s <= 2;
        when 2 => s <= 3;
        when five: 5 => s <= 7;
        when others => s <= 99;
    end generate;

end architecture;
