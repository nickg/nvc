package pack1 is
    function add1(x : in integer) return integer;
end package;

package body pack1 is

    function add1(x : in integer) return integer is
    begin
        return x + 1;
    end function;

end package body;
