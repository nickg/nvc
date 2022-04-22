package overflow is
    function add(x, y : integer) return integer;
    function sub(x, y : integer) return integer;
    function mul(x, y : integer) return integer;
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
end package body;
