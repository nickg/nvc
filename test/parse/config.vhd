configuration a of b is
    use work.foo;
    attribute x of y : signal is 5;
    for c
        use entity work.foo(x);
    end for
end configuration;
