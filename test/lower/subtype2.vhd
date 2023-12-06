package pack is
    function expensive(x : integer) return integer;
    subtype t_sub1 is bit_vector(1 to expensive(5));
    type t_rec is record
        f : t_sub1;
    end record;
end package;

package body pack is
    function expensive(x : integer) return integer is
    begin
        return x;
    end function;
end package body;

-------------------------------------------------------------------------------

entity subtype2 is
end entity;

use work.pack.all;

architecture test of subtype2 is
begin

    b1: block is
        port ( p : in t_sub1 );
        port map ( "10101" );
    begin
    end block;

    -- b2: block is
    --     port ( p : in t_rec );
    --     port map ( ( f => "10101" ) );
    -- begin
    -- end block;

end architecture;
