package issue760 is
    type t_array is array (natural range <>) of integer;
end package;

package body issue760 is
    -- Hides predef defined in package header
    impure function to_string (x : t_array) return string is
    begin
        return "";
    end function;
end package body;
