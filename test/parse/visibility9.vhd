package pack is
    type t_abc is (a, b, c);
    subtype t_ab is t_abc range a to b;

    function "=" (x, y : t_abc) return boolean;

    type t_xyz is (x, y, z);
end package;

-------------------------------------------------------------------------------

use work.pack.t_ab;
use work.pack.x;

entity visibility9 is
end entity;

architecture test of visibility9 is
    constant c1 : t_ab := a;            -- OK
    constant c2 : boolean := c1 = a;    -- OK
    constant c3 : boolean := x = x;     -- Error
begin

end architecture;
