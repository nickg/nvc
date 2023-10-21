package pack1 is
    type t_foo is (a, b, c);
    alias a_foo is t_foo;

    -- This causes a crash
    function "=" (l, r : t_foo) return boolean;
end package;

-------------------------------------------------------------------------------

package pack2 is
    generic ( type t );
end package;

-------------------------------------------------------------------------------

use work.pack1.all;

package pack3 is
    type t_type is (one, two, three);
    package pack2_inst is new work.pack2 generic map ( t_type );  -- OK
end package;
