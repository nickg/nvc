configuration conf of ent is
    use work.foo;
    attribute x of y : signal is 5;
    for arch
        for all : comp
            use entity work.foo(x);
        end for;
    end for;
end configuration;
