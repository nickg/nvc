package pack is
    type rec is record
        x, y : integer;
    end record;
end package;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub is
    generic ( r : rec );
    port ( o : out bit_vector(1 to 3) );
end entity;

architecture test of sub is
begin
    p1: o(1 to r.x) <= (others => '1');
end architecture;

-------------------------------------------------------------------------------

entity driver1 is
end entity;

use work.pack.all;

architecture test of driver1 is
    function get_rec return rec is
    begin
        return (2, 3);
    end function;

    constant c : rec := get_rec;
    signal s : bit_vector(1 to 3);
begin

    u: entity work.sub
        generic map ( c )
        port map ( s );

end architecture;
