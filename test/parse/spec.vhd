architecture a of b is

    for x : y use entity work.foo;

    for x1, x2 : y use entity work.foo;

    for x : y use entity work.foo(bar);

    for x : y use entity work.foo(bar)
        generic map ( a => 1 )
        port map ( b => g );

    for all : y use configuration yah;

    for others : y use open;

begin

end architecture;
