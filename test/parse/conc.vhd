entity e is end entity;
architecture a of e is
    signal x, a, b, c : bit;
    signal foo, bar   : boolean;
    signal y          : integer;
    signal v          : bit_vector(1 to 2);

    procedure pcall(x : in bit; y : in integer);
    procedure xxx;
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

    assert y = 5;

    (a, b) <= v;

    xxx;

end architecture;
