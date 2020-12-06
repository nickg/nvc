entity instance is end entity;

entity foo is
    port ( a, b, c : in integer; x : out integer );
end entity;

architecture goo of foo is
begin
end architecture;

configuration bar of foo is
    use work.foo;
    for goo
    end for;
end configuration;

architecture test of instance is
    component foo is
    end component;
    signal x, c : integer;
    signal s1, s2, s3 : integer;
begin

    a: foo;

    b: entity work.foo;

    b1: entity work.foo(goo);

    c1: configuration work.bar;

    d: component foo;

    e: entity work.foo
        port map ( s1, s2, s3 );

    f: entity work.foo
        port map ( s1, s2, x => s3 );

    g: entity work.foo
        generic map ( X => 1 )
        port map ( s1, s2 );

    h: entity work.foo
        port map ( a => open );

    i: foo port map ( x );

end architecture;
