package pack is
    type t_nat_vec is array (natural range <>) of natural;
    function count_drivers (inputs : t_nat_vec) return natural;
    subtype t_rnat is count_drivers natural;

    impure function get_limit return integer;
end package;

package body pack is
    function count_drivers (inputs : t_nat_vec) return natural is
    begin
        return inputs'length;
    end function;

    impure function get_limit return integer is
    begin
        return 3;
    end function;
end package body;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub is
    port ( o : out t_rnat );
end entity;

architecture three_drivers of sub is
    constant limit : integer := get_limit;
begin
    g: for i in 1 to limit generate
        o <= 6;
    end generate;
    postponed assert o = 3;
end architecture;

architecture no_drivers of sub is
begin
    postponed assert o = 0;
end architecture;

-------------------------------------------------------------------------------

entity driver19 is
end entity;

use work.pack.all;

architecture test of driver19 is
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
