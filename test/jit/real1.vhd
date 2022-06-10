package real1 is
    function approx(x, y : real; t : real := 0.001) return boolean;
end package;

package body real1 is

    function approx(x, y : real; t : real := 0.001) return boolean is
    begin
        return abs(x - y) < t;
    end function;

end package body;
