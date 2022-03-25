package config is
    constant width : integer;
end package;

-------------------------------------------------------------------------------

use work.config.all;

package types is
    type rec is record
        x : bit_vector(1 to width);     -- OK
    end record;
end package;

-------------------------------------------------------------------------------

package body config is
    constant width : integer := 6;
end package body;

-------------------------------------------------------------------------------

entity bounds31 is
end entity;

use work.types.all;

architecture test of bounds31 is
    signal r : rec;
begin

    main: process is
    begin
        assert r.x'length = 6;
        assert r = ( x => "000" ) severity warning;      -- Error
        wait;
    end process;

end architecture;
