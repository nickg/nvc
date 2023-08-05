package complex is
    type COMPLEX is
    record
        RE : REAL;                          -- Real part
        IM : REAL;                          -- Imaginary part
    end record;

    constant MATH_CBASE_1 : COMPLEX := COMPLEX'(1.0, 0.0);
    constant MATH_CBASE_J : COMPLEX := COMPLEX'(0.0, 1.0);
    constant MATH_CZERO   : COMPLEX := COMPLEX'(0.0, 0.0);

    function SQRT(Z: in COMPLEX ) return COMPLEX;
end package;

package body complex is

    function SQRT(Z: in COMPLEX ) return COMPLEX is
    begin
        return MATH_CZERO;
    end function;

end package body;
