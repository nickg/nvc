package config is
    constant width : integer;
end package;

-------------------------------------------------------------------------------

use work.config.all;

package types is
    type pair is record
        x, y : integer;
    end record;

    type pair_vec is array (natural range <>) of pair;

    type rec is record
        v : pair_vec(1 to width);     -- OK
    end record;
end package;

-------------------------------------------------------------------------------

package body config is
    constant width : integer := 2;
end package body;

-------------------------------------------------------------------------------

entity record27 is
end entity;

use work.types.all;

architecture test of record27 is
    signal r : rec;
begin

    main: process is
    begin
        assert r.v'length = 2;
        assert r = ( v => (others => (integer'left, integer'left) ) );
        r.v <= ( (1, 2), (3, 4) );
        wait for 1 ns;
        assert r = ( v => ( (1, 2), (3, 4) ) );
        r.v(2).x <= 7;
        wait for 1 ns;
        assert r = ( v => ( (1, 2), (7, 4) ) );
        wait;
    end process;

end architecture;
