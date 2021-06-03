package resolution is

    type my_utype is (a, b, c);
    type my_utype_vector is array (natural range <>) of my_utype;

    subtype my_type_error is bit_vector my_utype;  -- Error

    constant vec : bit_vector(1 to 3) := "101";

    subtype my_type_error_2 is vec(1) my_utype;  -- Error

    function resolved (s : my_utype_vector) return my_utype;

    subtype my_type is resolved my_utype;  -- OK
    subtype my_type_vector is (resolved) my_utype_vector;  -- OK

    subtype my_type_vector_2 is (resolved, resolved) my_utype_vector;  -- Error

    type urec is record
        x, y : integer;
    end record;

    type urec_vector is array (natural range <>) of urec;

    function resolved (s : urec_vector) return urec;

    subtype rec is resolved urec;       -- OK

    subtype my_type2 is (resolved) my_utype;  -- Error

    procedure proc (s : my_utype_vector);

    subtype my_type3 is proc my_utype;  -- Error

    function func1 (s : urec_vector) return my_utype;

    subtype my_type4 is func1 my_utype;  -- Error

    function func2 (s : my_utype_vector) return urec;

    subtype my_type5 is func2 my_utype;  -- Error

    type my_utype_vector_vector is array (natural range <>) of my_utype_vector(1 to 3);
    subtype my_type_vector_vector is ((resolved)) my_utype_vector_vector;

end package;
