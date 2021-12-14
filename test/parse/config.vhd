entity ent is
end entity;

entity foo is
    attribute x : integer;
end entity;

architecture arch of foo is
begin
end architecture;

architecture arch of ent is
    component comp is
    end component;
    component moo is
    end component;
begin
    blah: component moo;
    p: process is begin end process;
end architecture;

configuration conf2 of foo is
    for arch
    end for;
end configuration;

configuration conf of ent is
    use work.foo;
    attribute x of 'A' : literal is 5;
    for arch
        for all : comp
            use entity work.foo(arch);
        end for;
        for blah : moo use configuration work.conf2; end for;
    end for;
end configuration;

configuration bad1 of conf is           -- Error
    for arch
        for x : y use configuration work.conf; end for;  -- Suppressed error
    end for;
end configuration;

configuration bad2 of ent is
    for bad                             -- Error
        for x : y use configuration work.conf; end for;  -- Suppressed error
    end for;
end configuration;

configuration bad3 of ent is
    for arch
        for p                           -- Error
            for x : y use configuration work.conf; end for;  -- Suppressed error
        end for;
        for p : comp use configuration work.conf; end for;  -- Error
    end for;
end configuration;

package pack is
    component pack_comp is
    end component;
end package;

use work.pack.all;

architecture use_pack of ent is
begin
    sub: component pack_comp;
end architecture;

configuration use_pack_ent of ent is
    for use_pack
        for all : pack_comp use entity work.foo;
        end for;
    end for;
end configuration;
