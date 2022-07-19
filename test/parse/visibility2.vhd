package pack1 is
    procedure p (x : integer);
end package;

-------------------------------------------------------------------------------

use work.pack1.all;

package pack2 is
    procedure p (y : integer);          -- OK, hides pack1.p
end package;

package body pack2 is
    procedure p (y : integer) is        -- OK
    begin
    end procedure;
end package body;
