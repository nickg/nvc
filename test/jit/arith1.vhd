package arith1 is
    function div(x, y : integer) return integer;
    function div(x, y : real) return real;
    function div(x : integer; y : real) return real;

    function exp(x : real; y : integer) return real;
    function exp(x, y : integer) return integer;

    function neg(x : integer) return integer;
    function neg(x : real) return real;

    function cast(x : real) return integer;

    function abz(x : integer) return integer;
    function abz(x : real) return real;

    function modd(x, y : integer) return integer;
    function remm(x, y : integer) return integer;
end package;

package body arith1 is

    function div(x, y : integer) return integer is
    begin
        return x / y;
    end function;

    function div(x, y : real) return real is
    begin
        return x / y;
    end function;

    function div(x : integer; y : real) return real is
    begin
        return real(x) / y;
    end function;

    function exp(x : real; y : integer) return real is
    begin
        return x ** y;
    end function;

    function exp(x, y : integer) return integer is
    begin
        return x ** y;
    end function;

    function neg(x : integer) return integer is
    begin
        return -x;
    end function;

    function neg(x : real) return real is
    begin
        return -x;
    end function;

    function cast(x : real) return integer is
    begin
        return integer(x);
    end function;

    function abz(x : integer) return integer is
    begin
        return abs x;
    end function;

    function abz(x : real) return real is
    begin
        return abs x;
    end function;

    function modd(x, y : integer) return integer is
    begin
        return x mod y;
    end function;

    function remm(x, y : integer) return integer is
    begin
        return x rem y;
    end function;

end package body;
