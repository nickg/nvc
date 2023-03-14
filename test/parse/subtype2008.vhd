package subtype2008 is
    subtype sub1 is integer_vector;     -- integer_vector(open)
    subtype sub2 is integer_vector(open);  -- integer_vector(open)
    subtype sub3 is sub1;               -- sub1(open)

    type int2d is array (natural range <>) of integer_vector;

    subtype sub4 is int2d;              -- int2d(open)(open)
    subtype sub5 is int2d(1 to 2);      -- int2d(1 to 2)(open)
    subtype sub6 is sub5(1 to 3);       -- sub5(1 to 3)
    subtype sub7 is sub6;               -- sub6
end package;
