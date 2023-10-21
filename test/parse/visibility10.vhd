package pack1 is
    type t_type is (foo, bar, baz);
end package;

-------------------------------------------------------------------------------

package pack2 is
    alias t_type is work.pack1.t_type;
    alias octet is character;

    function foo (x : t_type) return integer;
end package;

-------------------------------------------------------------------------------

use work.pack2.all;

package pack3 is
    constant k : integer := foo(foo);   -- OK
    constant s : string := "hello";     -- OK
end package;
