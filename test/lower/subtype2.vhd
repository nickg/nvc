package pack1 is
    function expensive(x : integer) return integer;
end package;

package body pack1 is
    function expensive(x : integer) return integer is
    begin
        return x;
    end function;
end package body;

-------------------------------------------------------------------------------

use work.pack1.all;

package pack2 is
    subtype t_sub1 is bit_vector(1 to expensive(5));
    type t_rec is record
        f : t_sub1;
    end record;
    type t_nest1 is array (natural range <>) of t_sub1;
    subtype t_integer is integer range 0 to expensive(5);
    subtype t_real is real range 0.0 to real(expensive(99));
end package;

-------------------------------------------------------------------------------

entity subtype2 is
end entity;

use work.pack2.all;

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

    p1: process is
        variable a : t_nest1(1 to 3);
    begin
        a(1) := "101";
        wait;
    end process;

    p2: process is
        variable x : t_integer;
    begin
        x := 5;
        wait;
    end process;

    -- p3: process is
    --     variable x : t_real;
    -- begin
    --     x := 5.0;
    --     wait;
    -- end process;

end architecture;
