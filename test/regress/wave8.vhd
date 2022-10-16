package pack is
    type rec is record
        x : integer;
    end record;

    impure function get_field (r : rec) return integer;
end package;

package body pack is
    impure function get_field (r : rec) return integer is
    begin
        return r.x;
    end function;
end package body;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub is
    generic ( r : rec );
    port ( o : out bit_vector(1 to get_field(r)) );
end entity;

architecture test of sub is
begin

end architecture;

-------------------------------------------------------------------------------

entity wave8 is
end entity;

use work.pack.all;

architecture test of wave8 is
    function new_rec return rec is
    begin
        return (x => 4);
    end function;

    constant r : rec := new_rec;
    signal x : bit_vector(1 to 4);
begin

    u: entity work.sub
        generic map ( r )
        port map ( x );

end architecture;
