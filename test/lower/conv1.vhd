package conv1 is
    type rec is record
        x : integer;
    end record;

    type rec_array is array (natural range <>) of rec;

    function get(a : rec_array) return real;
end package;

package body conv1 is
    function get(a : rec_array) return real is
    begin
        return real(a(0).x);            -- Must reify a(0).x
    end function;
end package body;
