package ieeewarn is
    constant enabled : boolean;
end package;

library nvc;
use nvc.sim_pkg.all;

package body ieeewarn is
    constant enabled : boolean := ieee_warnings;  -- Should not be folded
end package body;
