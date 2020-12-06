package interfaces is

    type text is file of string;

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

    component comp6 is
        generic (signal   p : integer); -- Error
    end component;

    component comp7 is
        generic (variable p : integer); -- Error
    end component;

    component comp8 is
        generic (file     p : text);    -- Error
    end component;

    procedure proc1(c : buffer integer);    -- Error
    procedure proc2(c : linkage integer);   -- Error

    procedure proc3(constant c : inout integer);    -- Error
    procedure proc3a(constant c : out   integer);    -- Error

    procedure proc4(file c : integer);              -- Error
    procedure proc5(constant c : text);             -- Error

end package interfaces;
