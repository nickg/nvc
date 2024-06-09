package defer1 is
    function func1 return integer;
    constant c1 : integer := func1;     -- Error
end package;
