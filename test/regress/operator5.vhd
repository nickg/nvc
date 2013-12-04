package pack is
    type int_vec2 is array (integer range <>) of integer;
    type int_vec is array (integer range <>) of integer;

    function "<"(a, b : int_vec) return boolean;

end package;

package body pack is

    function "<"(a, b : int_vec) return boolean is
    begin
        return false;
    end function;

end package body;

entity operator5 is
end entity;

use work.pack.all;

architecture test of operator5 is

    function ">="(a, b : int_vec) return boolean is
    begin
        return false;
    end function;

begin

    process is
        variable x, y : int_vec(1 to 3);
    begin
        x := (1, 2, 3);
        y := (4, 5, 6);
        assert not (y >= x);
        assert (int_vec2(y) >= int_vec2(x));
        assert not (y < x) and not (x < y);
        wait;
    end process;

end architecture;
