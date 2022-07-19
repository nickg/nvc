package string_list is
    type string_list;

    type string_list is record
        f : integer;
    end record;
end package;

-------------------------------------------------------------------------------

use work.string_list.all;

package pack2 is
    procedure test (x : inout string_list);  -- OK
end package;
