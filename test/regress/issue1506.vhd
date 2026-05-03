entity sub is
    generic (
        n: positive;
        foo: integer_vector(0 to n - 1);
        bar: integer_vector(foo'range)

    );
end;

architecture rtl of sub is
    signal res, i: integer := 0;
begin
    res <= bar(i);
end;

--------------------------------------------------------------------------------

entity issue1506 is
end;

architecture rtl of issue1506 is
begin
    sub : entity work.sub
    generic map (
        2,
        (0, 1),
        (0, 1)
    );
end;