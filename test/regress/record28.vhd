package config is
    constant width : integer;
end package;

-------------------------------------------------------------------------------

use work.config.all;

package types is
    -- type pair is record
    --     x, y : integer;
    -- end record;

    -- type pair_vec is array (natural range <>) of pair;

    type rec is record
        v : bit_vector(1 to width);     -- OK
    end record;
end package;

-------------------------------------------------------------------------------

package body config is
    constant width : integer := 2;
end package body;

-------------------------------------------------------------------------------

entity record28 is
end entity;

use work.types.all;

architecture test of record28 is
    signal r, s : rec;
begin

    main: process is
    begin
        assert r.v'length = 2;
        r <= ( v => "10" ) after 1 ns;
        wait for 2 ns;
        assert s.v = "10";
        wait;
    end process;

    process (r) is
    begin
        s <= r;
    end process;

end architecture;
