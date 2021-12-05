package pack is
    function get_elt(index : natural) return integer;
end package;

package body pack is

    type int_vector is array (natural range <>) of integer;

    constant arr : int_vector(1 to 5) := (10, 20, 30, 40, 50);
    constant c1  : integer := 40 + 2;

    function get_elt(index : natural) return integer is
    begin
        return arr(index);
    end function;

    function nested_get_elt(index : natural) return integer is
        function inner return integer is
        begin
            return arr(index);
        end function;
    begin
        return inner;
    end function;

end package body;
