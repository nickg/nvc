package pack is
    function add1(x : integer) return integer;
end package;

package body pack is
    function add1(x : integer) return integer is
    begin
        return x + 1;
    end function;

    function add1(x : real) return real is
    begin
        return x + 1.0;
    end function;
end package body;
