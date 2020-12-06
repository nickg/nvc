package func is

    function add(x, y : integer; z : in integer) return integer;

    impure function naughty return integer;

    function "+"(x, y : integer) return integer;

end package;

package body func is

    function "+"(x, y : integer) return integer is
    begin
        return 42;
    end function "+";

end package body;
