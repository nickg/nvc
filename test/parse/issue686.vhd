package pack1 is
    type t is (a, b);
end package;

-------------------------------------------------------------------------------

package pack2 is
    generic (type g);
end package;

-------------------------------------------------------------------------------

use work.pack1;

package pack3 is new work.pack2 generic map ( pack1.t );
