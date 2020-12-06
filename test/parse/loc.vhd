entity e is end entity;

architecture a of e is
    procedure f(x, y, z : integer);
    signal x : bit;
begin
    f(1, 2, 3 + 5);
    assert x'active;
end architecture;
