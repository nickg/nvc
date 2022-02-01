package p1 is
    type my_type is (a, b, c);
    type my_vec is array (natural range <>) of my_type;
    -- Implicit <= declared here
end package;

use work.p1.all;

package p2 is
    function "<=" (l, r : my_vec) return boolean;
end package;

entity e is
end entity;

use work.p1.all;
use work.p2.all;

architecture a of e is
begin

    process is
        variable x, y : my_vec(1 to 3);
    begin
        assert x <= y;                  -- OK (2008), Error (1993)
    end process;

end architecture;
