package pack1 is
    type t_foo is (a, b, c);

    function to_hstring (x : t_foo) return string;
    alias to_hex_string is to_hstring [t_foo return string];
end package;

-------------------------------------------------------------------------------

use work.pack1.all;

package pack2 is
    alias t_foo2 is t_foo;
end package;

-------------------------------------------------------------------------------

use work.pack1.all;
use work.pack2.all;

package pack3 is
    constant s : string := to_hex_string(a);  -- OK
end package;
