entity e is
end entity;

architecture a1 of e is
    attribute foo : integer;
    attribute bar : string;

    signal x, y, z : integer;

    attribute foo of x : signal is 6;   -- OK
    attribute bar of y : signal is "hello";  -- OK
begin

    process is
        variable v : integer;
    begin
        v := x'foo;                     -- OK
        report y'bar;                   -- OK
    end process;

    process is
    begin
        report z'foo;                   -- Error
    end process;
    
end architecture;
    
architecture a2 of e is
    attribute foo : integer;
    attribute bar : string;

    signal x, y, z : integer;

    attribute foo of z : signal is string'("boo");  -- Error
    attribute bar of x : signal is 73;  -- Error
    attribute foo of q : signal is 71;  -- Error
begin
end architecture;
