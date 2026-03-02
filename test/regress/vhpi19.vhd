package types is
    type t_freq is range 1 to 1000000
        units
            hz;
            khz = 1000 hz;
            mhz = 1000 khz;
        end units;
end package;

-------------------------------------------------------------------------------

entity vhpi19 is
end entity;

use work.types.all;

architecture test of vhpi19 is
begin
end architecture;
