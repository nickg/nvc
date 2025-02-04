package p1 is
    constant c1 : integer := 5;
end package;

-------------------------------------------------------------------------------

use work.p1.all;

package p2 is
    constant c2 : integer := c1;
end package;

-------------------------------------------------------------------------------

package p1 is
    constant c1 : integer := 6;
end package;

-------------------------------------------------------------------------------

entity order4 is
end entity;

use work.p1.all;
use work.p2.all;

architecture test of order4 is
begin

    assert c1 = 6;
    assert c2 = 6;

end architecture;
