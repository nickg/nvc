package pkg is

    type p1 is protected
    end protected;

end package;

package body pkg is

    type p1 is protected body
    end protected body;

end package body;

entity e is
end entity;

use work.pkg.all;

architecture a of e is
    type p2 is protected
    end protected;

    type p2 is protected body
    end protected body;

    shared variable t : p1;
    shared variable s : p2;
begin
end architecture;
