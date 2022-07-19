package pack1 is
    type t is (foo, bar, baz);
    alias a is t;
end package;

-------------------------------------------------------------------------------

use work.pack1.all;

package pack2 is
    constant k : t := foo;              -- OK
    procedure test1;
end package;

package body pack2 is

    function height (height : integer) return integer is
    begin
        return height * 2;
    end function;

    procedure test1 is
    begin
        assert height ( height => 1 ) = 2;  -- OK
    end procedure;

end package body;
