entity c1_ent1 is
end entity;

-------------------------------------------------------------------------------

package p is
end package;

-------------------------------------------------------------------------------

entity e is
end entity;

architecture a of e is

    component c1 is
    end component;

    component c2 is
    end component;

    for i1: c2 use entity work.c1_ent1;   -- Error

    for i1: e use entity work.c1_ent1;    -- Error

    for i1: c1 use entity work.c1_ent1;   -- OK

    for bad: c1 use entity work.c1_ent1;  -- Error

    for i2: c1 use entity work.c1_ent1;   -- Error

    for i1: c1 use entity work.c1_ent1;   -- Error

    for i3: c1 use entity work.not_here;  -- Error

    for i3: c1 use entity work.p;         -- Error

begin

    i1: component c1;

    i2: entity work.e;

    i3: component c1;

end architecture;
