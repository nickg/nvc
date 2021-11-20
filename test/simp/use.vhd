-- Library somelib

package pack is
    constant N : natural := 42;
end package;

use work.pack.all;                      -- Should get rewritten to somelib.pack

package pack2 is
    constant X : natural := N;
end package;
