package pkg is
    subtype s is integer (0 to 10);       -- error
    subtype ss is string range 2 to 5;    -- error
end package pkg;
