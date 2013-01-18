architecture a of e is
begin

    x <= a or b;

    x <= 1 when foo
         else 2 when bar
         else 3;

    with y select x <=
        1 when a,
        2 when b,
        3 when others;

    pcall(x, y);

    assert x = 5;

end architecture;
