package my_pkg is
    function default_value_function return integer;

    function my_function(
        a : integer := default_value_function
    )
    return integer;
end package my_pkg;

package body my_pkg is
    function default_value_function return integer is
    begin
        return 0;
    end function default_value_function;

    function my_function(
        a : integer := default_value_function
    ) return integer is
    begin
        return 22;
    end function my_function;
end package body my_pkg;
