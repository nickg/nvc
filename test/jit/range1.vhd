package range1 is
    subtype preal is real range 1.0 to real'high;

    function as_positive (x : integer) return positive;
    function as_positive (x : real) return preal;
end package;

package body range1 is

    function as_positive (x : integer) return positive is
        variable p : positive;
    begin
        p := x;
        return p;
    end function;

    function as_positive (x : real) return preal is
        variable p : preal;
    begin
        p := x;
        return p;
    end function;

end package body;
