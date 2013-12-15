package p is
    -- Reduced failure case from VESTs
    type    ibi    is array (integer range <>, boolean range <>) of integer ;
    subtype irange is integer range 1 to 5 ;
    subtype brange is boolean range false to true ;
    subtype ibi_s  is ibi (irange, brange);

    -- TODO
    subtype ibi_x  is ibi (brange, irange);  -- Error
end package;
