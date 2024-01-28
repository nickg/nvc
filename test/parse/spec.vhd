entity b is end entity;

entity foo is
    generic ( a : integer := 2 );
    port ( b : in integer := 5 );
end entity;

architecture arch of foo is
begin
end architecture;

configuration yah of foo is
    use work.foo;
    for arch
    end for;
end configuration;

architecture a of b is

    component y is end component;
    component p is end component;

    for x : y use entity work.foo;

    for x1, x2 : y use entity work.foo;

    for x3 : y use entity work.foo(arch);

    for x4 : y use entity work.foo(arch)
        generic map ( a => 1 )
        port map ( b => 6 );

    for all : p use configuration work.yah;

    for others : y use open;

begin

    x: component y;
    x1: component y;
    x2: component y;
    x3: component y;
    x4: component y;

end architecture;
