package pack is
    function get_elt(index : natural) return integer;
    function nested_get_elt(index : natural) return integer;
    procedure read_elt(index : natural; result : out integer);
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

    procedure read_elt(index : natural; result : out integer) is
    begin
        if index > 100 then
            wait for 1 ns;              -- Forces heap allocation
        end if;
        result := get_elt(index);
    end procedure;

end package body;
