package interfaces is

    component comp1 is
        port (
        signal   p1 : integer;   -- OK
                 p2 : integer    -- OK
        );
    end component;

    component comp2 is
        port (variable p : integer);    -- Error
    end component;

    component comp3 is
        port (constant p : integer);    -- Error
    end component;

    component comp4 is
        port (file     p : text);       -- Error
    end component;

    component comp5 is
        generic (constant p1 : integer; -- OK
                          p2 : integer);-- OK
    end component;

    component comp5 is
        generic (signal   p : integer); -- Error
    end component;

    component comp5 is
        generic (variable p : integer); -- Error
    end component;

    component comp5 is
        generic (file     p : text);    -- Error
    end component;

    procedure proc1(c : buffer integer);    -- Error
    procedure proc2(c : linkage integer);   -- Error

end package interfaces;
