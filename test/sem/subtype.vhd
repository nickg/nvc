package p is
    -- Reduced failure case from VESTs
    type    ibi    is array (integer range <>, boolean range <>) of integer ;
    subtype irange is integer range 1 to 5 ;
    subtype brange is boolean range false to true ;
    subtype ibi_s  is ibi (irange, brange);  -- OK


    subtype ibi_x  is ibi (brange, irange);  -- Error


    type int_vec is array (integer range <>) of integer;
    function resolved(x : int_vec) return integer;

    signal rint1 : resolved integer;  -- OK
    signal rint2 : not_here integer;  -- Error

    constant k : integer := 5;
    type bit_vector_array is array (natural range <>) of bit_vector(k downto 0);

end package;
