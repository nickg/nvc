package pack1 is
    type t is (foo, bar, baz);
    type p is range 0 to 100
        units
            one;
            ten = 10 one;
        end units;
end package;

use work.pack1.t;
use work.pack1.p;

package pack2 is
    constant c : t := foo;              -- OK
    constant k : p := 5 one;            -- Ok
end package;
