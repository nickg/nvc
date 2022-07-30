package pack is generic ( x : string := "foo" ) ;
    constant s : string := x ;
    constant t : string := "hello" ;
    constant k : integer;
end package ;

package body pack is
    constant k : integer := 42;
end package body;

-------------------------------------------------------------------------------

package pack2 is new work.pack generic map (x => "bar") ;

-------------------------------------------------------------------------------

entity genpack1 is
end entity;

use work.pack2.all;

architecture test of genpack1 is
    function get_length (s : string) return integer is
    begin
        return s'length;
    end function;
begin

    p1: process is
    begin
        assert k = 42;
        assert s = "bar";
        assert t = "hello";
        wait;
    end process;

end architecture;
