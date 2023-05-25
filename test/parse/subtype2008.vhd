package subtype2008 is
    subtype sub1 is integer_vector;     -- integer_vector(open)
    subtype sub2 is integer_vector(open);  -- integer_vector(open)
    subtype sub3 is sub1;               -- sub1(open)

    type int2d is array (natural range <>) of integer_vector;

    subtype sub4 is int2d;              -- int2d(open)(open)
    subtype sub5 is int2d(1 to 2);      -- int2d(1 to 2)(open)
    subtype sub6 is sub5(1 to 3);       -- sub5(1 to 3)
    subtype sub7 is sub6;               -- sub6

    type rec is record
        f : integer_vector;
        g : int2d;
    end record;

    subtype sub8 is rec;                -- rec(f(open), g(open)(open))
    subtype sub9 is rec(g(1 to 3));     -- rec(f(open), g(1 to 3)(open))
    subtype sub10 is sub9(g(1 to 5));   -- rec(f(open), g(1 to 3)(1 to 5))

    type rec_array is array (natural range <>) of rec;

    subtype sub11 is rec_array(1 to 9);  -- rec_array(1 to 9)(f(open), g(open)(open))
end package;
