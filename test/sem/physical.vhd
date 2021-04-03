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

end package;
