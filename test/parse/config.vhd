entity ent is
end entity;

entity foo is
    attribute x : integer;
end entity;

configuration conf2 of foo is
    for arch
    end for;
end configuration;

configuration conf of ent is
    use work.foo;
    attribute x of 'A' : literal is 5;
    for arch
        for all : comp
            use entity work.foo(x);
        end for;
        for blah : moo use configuration work.conf2; end for;
    end for;
end configuration;
