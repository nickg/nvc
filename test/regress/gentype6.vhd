package pack1 is
    type t is (a, b, c);
    function "=" (l, r : t) return boolean;
end package;

package body pack1 is
    function "=" (l, r : t) return boolean is
    begin
        report "user-defined = called";
        return false;
    end function;
end package body;

-------------------------------------------------------------------------------

package pack2 is
    generic (type g);
    function equal (l, r : g) return boolean;
end package;

package body pack2 is
    function equal (l, r : g) return boolean is
    begin
        return l = r;
    end function;
end package body;

-------------------------------------------------------------------------------

entity gentype6 is
end entity;

use work.pack1.all;

architecture test of gentype6 is
    package pack3 is new work.pack2 generic map (g => t);
begin

    check: process is
    begin
        assert pack3.equal(a, a);
        wait;
    end process;

end architecture;
