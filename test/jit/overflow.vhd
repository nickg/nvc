package overflow is
    function add(x, y : integer) return integer;
    function sub(x, y : integer) return integer;
    function mul(x, y : integer) return integer;
    function exp(x, y : integer) return integer;

    type uint8 is range 0 to 255;

    function add(x, y : uint8) return uint8;
    function sub(x, y : uint8) return uint8;
    function mul(x, y : uint8) return uint8;
    function exp(x : uint8; y : integer) return uint8;
end package;

package body overflow is
    function add(x, y : integer) return integer is
    begin
        return x + y;
    end function;

    function sub(x, y : integer) return integer is
    begin
        return x - y;
    end function;

    function mul(x, y : integer) return integer is
    begin
        return x * y;
    end function;

    function exp(x, y : integer) return integer is
    begin
        return x ** y;
    end function;

    function add(x, y : uint8) return uint8 is
    begin
        return x + y;
    end function;

    function sub(x, y : uint8) return uint8 is
    begin
        return x - y;
    end function;

    function mul(x, y : uint8) return uint8 is
    begin
        return x * y;
    end function;

    function exp(x : uint8; y : integer) return uint8 is
    begin
        return x ** y;
    end function;
end package body;
