architecture test of instance is
begin

    a: foo;

    b: entity work.foo;

    b1: entity work.foo(goo);

    c: configuration work.bar;

    d: component foo;

    e: entity work.foo
        port map ( a, b, c );

    f: entity work.foo
        port map ( a, b, x => c );

    g: entity work.foo
        generic map ( X => 1 )
        port map ( a, b );            
    
end architecture;
