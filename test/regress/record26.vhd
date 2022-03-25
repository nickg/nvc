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

entity record26 is
end entity;

use work.types.all;

architecture test of record26 is
    signal r : rec;
begin

    main: process is
    begin
        assert r.x'length = 6;
        assert r = ( x => "000000" );
        r.x <= "101000";
        wait for 1 ns;
        assert r = ( x => "101000" );
        wait;
    end process;

end architecture;
