package pack is
    type int_vector is array (natural range <>) of natural;

    function spread_ints (x : integer) return int_vector;
end package;

package body pack is

    function spread_ints (x : integer) return int_vector is
        variable r : int_vector(1 to 5);
    begin
        for i in r'range loop
            r(i) := x;
        end loop;
        return r;
    end function;

end package body;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub is
    port ( o : out integer := 0 );
end entity;

architecture test of sub is
begin

    p1: process is
    begin
        o <= 1;
        wait for 1 ns;
        o <= 2;
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity bounds23 is
end entity;

use work.pack.all;

architecture test of bounds23 is
    signal x : int_vector(1 to 3);
begin

    uut: entity work.sub
        port map ( spread_ints(o) => x );  -- Error

end architecture;
