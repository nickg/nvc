package value1 is
    function str_to_int (x : string) return integer;
    function str_to_real (x : string) return real;
    function str_to_time (x : string) return time;
    function str_to_bool (x : string) return boolean;
end package;

package body value1 is

    function str_to_int (x : string) return integer is
    begin
        return integer'value(x);
    end function;

    function str_to_real (x : string) return real is
    begin
        return real'value(x);
    end function;

    function str_to_time (x : string) return time is
    begin
        return time'value(x);
    end function;

    function str_to_bool (x : string) return boolean is
    begin
        return boolean'value(x);
    end function;

end package body;
