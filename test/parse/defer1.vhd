package defer1 is
    function func1 return integer;
    constant c1 : integer := func1;     -- Error
    constant c2 : integer;
    constant c3 : integer := c2 + 1;    -- Error
    function func2 (x : integer := c2) return integer;  -- OK
end package;
