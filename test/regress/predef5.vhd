package pack is
    type t_abc is (a, b, c);
    subtype t_sub is t_abc range a to c;

    function "=" (x, y : t_abc) return boolean;
end package;

package body pack is
    function "=" (x, y : t_abc) return boolean is
    begin
        report "called overloaded =";
        return true;
    end function;
end package body;

-------------------------------------------------------------------------------

use work.pack.t_sub;

entity predef5 is
end entity;

architecture test of predef5 is
begin

    tb: process is
        variable x : t_sub := a;
    begin
        wait for 1 ns;
        assert x = c;
        wait;
    end process;

end architecture;
