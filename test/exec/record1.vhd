package pack is
    type rec is record
        x : integer;
        y : integer;
        z : integer;
    end record;

    constant r : rec;
end package;

package body pack is
    constant r : rec := (1, 2, 3);
end package body;

-------------------------------------------------------------------------------

package pack2 is
    function sum_fields return integer;
end package;

use work.pack.all;

package body pack2 is
    function sum_fields return integer is
    begin
        return r.x + r.y + r.z;
    end function;
end package body;
