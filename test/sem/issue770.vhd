package pkg is
    function identity_a (a : bit) return bit;
    function identity_b (b : bit) return bit;
end package pkg;

package body pkg is
    function identity_a (constant a : bit) return bit is
    begin
        return a;
    end function;

    function identity_b (constant b : bit) return bit is
    begin
        return b;
    end function;
end package body pkg;
