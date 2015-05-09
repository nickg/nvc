-- lib2
package pkg2 is
  constant const2 : integer := 1;
end package;

package body pkg2 is
end package body;

use work.pkg2.all;

package pkg is
  constant const : integer := const2 + 1;
end package;

package body pkg is
end package body;

-------------------------------------------------------------------------------
-- lib
library lib2;
use lib2.pkg.all;

entity ent is
end entity;

architecture a of ent is
begin
  main : process
  begin
    assert const = 2;
    wait;
  end process;
end architecture;
