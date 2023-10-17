entity conv is
end entity;

architecture test of conv is
    type e is (F, G, H);
    type a is array (integer range <>) of integer;
    type b is array (integer range <>) of integer;
    type c is array (integer range <>) of e;
    subtype d is a(1 to 3);
    type k is array (e range <>) of integer;
    type a2 is array (integer range <>, integer range <>) of integer;

    function eq(x : a; y : b) return boolean is
    begin
        return x = a(y);
    end function;

begin

    process is
        variable x : integer;
        variable y : a(1 to 3);
        variable z : b(1 to 3);
        variable w : c(1 to 3);
        variable u : d;
        variable t : time;
        variable r : real;
        variable s : k(f to h);
        variable y2 : a2(1 to 3, 1 to 3);
    begin
        x := integer(2);                -- OK
        x := integer(y);                -- Error
        y := z;                         -- Error
        y := a(z);                      -- OK
        w := c(y);                      -- Error
        assert eq(y, z);                -- OK
        assert eq(y, b(y));             -- OK
        assert eq(u, z);                -- OK
        assert eq(a(u), z);             -- OK
        r := real(sec / t);             -- OK
        x := integer(y(1));             -- OK
        y := a(s);                      -- Error
        y2 := a2(y);                    -- Error
        wait;
    end process;

end architecture;
