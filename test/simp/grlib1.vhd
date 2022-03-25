package config_types is
    constant cfg_width : integer := 1;
    type config_vec_t is array (0 to 16) of integer;
end package;

-------------------------------------------------------------------------------

use work.config_types.all;

package config is
    constant cfg_vec : config_vec_t := (
        cfg_width => 2,
        others => 0);
end package;

-------------------------------------------------------------------------------

use work.config.all;
use work.config_types.all;

package types is
    constant width : integer := 2 * cfg_vec(cfg_width);

    type rec is record
        x : bit_vector(1 to width);
    end record;
end package;
