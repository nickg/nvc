entity e is
end entity;

architecture a1 of e is
    attribute foo : integer;
    attribute bar : string;

    signal x, y, z : integer;

    attribute foo of x : signal is 6;   -- OK
    attribute bar of y : signal is "hello";  -- OK

    type int_vec is array (integer range <>) of integer;
    type int_vec_ptr is access int_vec;

    signal i : int_vec(1 to 3);

    attribute foo of i : signal is 6;  -- OK
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

    process is
        variable v : int_vec_ptr;
    begin
        assert v'length = 5;
        assert v.all'length = 62;
    end process;

    process is
    begin
        report e'path_name;             -- OK
        report e'instance_name;         -- OK
        report a1'path_name;            -- OK
        report a1'instance_name;        -- OK
    end process;

    process is
    begin
        assert i'event;                 -- OK
        assert i(1)'event;              -- OK
        assert i(x)'event;              -- Error
        assert i'foo = 1;               -- OK
        assert i(1)'foo = 2;            -- Error
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
