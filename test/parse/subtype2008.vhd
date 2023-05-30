package subtype2008 is
    subtype sub1 is integer_vector;     -- array (open) of integer
    subtype sub2 is integer_vector(open);  -- array (open) of integer
    subtype sub3 is sub1;               -- array (open) of integer

    type int2d is array (natural range <>) of integer_vector;

    subtype sub4 is int2d;              -- array (open) of array (open) of integer
    subtype sub5 is int2d(1 to 2);      -- array (1 to 2) of array (open) of integer
    subtype sub6 is sub5(open)(1 to 3); -- array (1 to 2) of array (1 to 3) of integer
    subtype sub7 is sub6;               -- array (1 to 2) of array (1 to 3) of integer

    type rec is record
        f : integer_vector;
        g : int2d;
    end record;

    subtype sub8 is rec;                -- rec(f(open), g(open)(open))
    subtype sub9 is rec(g(1 to 3));     -- rec(f(open), g(1 to 3)(open))
    subtype sub10 is sub9(g(open)(1 to 5));   -- rec(f(open), g(1 to 3)(1 to 5))

    type rec_array is array (natural range <>) of rec;

    subtype sub11 is rec_array(1 to 9);  -- array (1 to 9) of rec(f(open), g(open)(open))
end package;
