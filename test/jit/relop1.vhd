package relop1 is
    type uint8 is range 0 to 255;

    function cmpless (x, y : uint8) return boolean;
    function cmpless (x, y : real) return boolean;
end package;

package body relop1 is

    function cmpless (x, y : uint8) return boolean is
    begin
        return x < y;
    end function;

    function cmpless (x, y : real) return boolean is
    begin
        return x < y;
    end function;

end package body;
