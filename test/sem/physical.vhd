package physical is

    type mytime is range 1 to 30
        units
            fs;
            ps = 1000 fs;
            ns = 1000.0 ps;             -- Error
            foo = fs;                   -- OK
        end units;

    type myothertime is range 1 to 100
        units
            fs;                         -- OK? GHDL reports error
            bar = 100 foo;              -- Error
        end units;

    type t_freq is range 1 to 1000000
        units
            hz;
            khz = 1000 hz;
            mhz = 1000 khz;
        end units;

    constant c1 : integer := mhz / hz;  -- OK (special implicit conversion rule)

    function foo (x, y : integer) return integer;
    function foo (x : integer; y : string) return integer;

    constant c2 : integer := foo(mhz / hz, hz / hz);  -- OK

end package;
