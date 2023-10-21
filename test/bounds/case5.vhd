package pack1 is
    type t_type is (foo, bar, baz);
end package;

-------------------------------------------------------------------------------

package pack2 is
    alias t_type is work.pack1.t_type;
end package;

-------------------------------------------------------------------------------

use work.pack2.all;

package pack3 is
    procedure test1 (x : t_type);
end package;

package body pack3 is
    procedure test1 (x : t_type) is
    begin
        case x is                       -- Error
            when foo | bar => null;
        end case;
    end procedure;
end package body;
