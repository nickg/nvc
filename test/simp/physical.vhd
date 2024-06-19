package pack is
    constant c1 : time := 1000 hr;      -- Error
    constant c2 : time := 42141.4 hr;   -- Error
    constant c3 : time := 8.2 ms;       -- OK
end package;
