package ieeewarn is
    constant enabled : boolean;
end package;

library nvc;
use nvc.sim_pkg.all;

package body ieeewarn is
    constant enabled : boolean := ieee_warnings;  -- Should not be folded
end package body;

-------------------------------------------------------------------------------

entity ieeewarn_e is
end entity;

use work.ieeewarn;

architecture a of ieeewarn_e is
    constant e : boolean := ieeewarn.enabled;  -- Also shouldn't be folded
begin
end architecture;
