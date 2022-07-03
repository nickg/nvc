package typepack is
    type foo is (A, B, C);
    function "=" (l, r : foo) return boolean;
end package;

package body typepack is
    function "=" (l, r : foo) return boolean is
    begin
        return false;
    end function;
end package body;

package genpack is
    generic ( type t; def : integer );
end package;

package genpack_bitvec is new work.genpack
    generic map (t => bit_vector, def => 2);

use work.typepack.all;

package genpack_foo is new work.genpack
    generic map (t => foo, def => 2);

use work.genpack_foo.all;
use work.typepack.all;

entity e is
end entity;

architecture test of e is
begin

    p1: process is
        variable x, y : foo;
    begin
        assert x = y;    -- OK
        wait;
    end process;

    p2: process is
        variable x : t;  -- OK
        variable y : integer := def;
    begin
    end process;

    p3: process is
        use work.genpack.all;  -- Error
        use work.genpack;               -- OK
    begin
    end process;

end architecture;
