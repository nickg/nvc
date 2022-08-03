entity e is end entity;
architecture a of e is
    signal x : integer;
    component c is end component;
    attribute foo : integer;
    attribute foo of x : signal is 5;
    attribute foo of c : component is 5;
    type t is ('0', '1');
    attribute foo of '1' : literal is 6;
    attribute foo of t : type is 4;
    attribute foo of integer : type is 4;  -- Error
    attribute foo of a1 : label is 6;
    attribute foo of e : label is 6;   -- Error
begin
    a1: assert x'foo = 5;
    a2: assert a1'foo = 6;
end architecture;
