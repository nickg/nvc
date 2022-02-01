entity e is end entity;
architecture a of e is
    signal x : integer;
    component c is end component;
    attribute foo : integer;
    attribute foo of x : signal is 5;
    attribute foo of c : component is 5;
    attribute foo of '1' : literal is 6;
    attribute foo of integer : type is 4;
begin
    assert x'foo = 5;
end architecture;
