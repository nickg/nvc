entity instance is end entity;

entity foo is
    port ( a, b, c : in integer := 0; x : out integer );
end entity;

architecture goo of foo is
begin
end architecture;

configuration bar of foo is
    use work.foo;
    for goo
    end for;
end configuration;

package p is
    component c is
        port ( x : bit );
    end component;
end package;

entity something is
    port ( a : in bit := '0' ) ;
end entity ;

use work.p;
use work.something;

architecture test of instance is
    component foo is
    end component;
    signal x, c : integer;
    signal s1, s2, s3 : integer;
    signal v : bit_vector(1 to 3);
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

    j: work.p.c port map ( '1' );

    k: v(1) port map ( x );

    l: entity something;

    m: component something;             -- Error

    n: configuration something;         -- Error

end architecture;
