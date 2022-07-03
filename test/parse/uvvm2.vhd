-- Alias of enum in another package
--
package pack1 is
    type t is (foo, bar, baz);
end package;

use work.pack1.all;

package pack2 is
    alias t is work.pack1.t;            -- OK
end package;

use work.pack2.all;

package pack3 is
    constant c1 : t := foo;             -- OK
end package;

use work.pack1.all;
use work.pack2.all;

package pack4 is
    constant c2 : t := bar;             -- OK
    constant c3 : integer := foo;       -- Error
end package;
