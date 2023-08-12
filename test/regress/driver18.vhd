package pack is
    type t_nat_vec is array (natural range <>) of natural;
    function count_drivers (inputs : t_nat_vec) return natural;
    subtype t_rnat is count_drivers natural;
end package;

package body pack is
    function count_drivers (inputs : t_nat_vec) return natural is
    begin
        return inputs'length;
    end function;
end package body;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub is
    port ( o : out t_rnat );
end entity;

architecture three_drivers of sub is
begin
    o <= 6;
    o <= 52;
    o <= 99;
    postponed assert o = 3;
end architecture;

architecture no_drivers of sub is
begin
    postponed assert o = 0;
end architecture;

-------------------------------------------------------------------------------

entity driver18 is
end entity;

use work.pack.all;

architecture test of driver18 is
    signal s : t_rnat;
begin

    u1: entity work.sub(three_drivers)
        port map ( s );

    u2: entity work.sub(no_drivers)
        port map ( s );

    s <= 777;
    s <= 100;

    postponed assert s = 4;

end architecture;
