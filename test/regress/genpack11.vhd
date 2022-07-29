package pack is generic ( x : string := "foo" ) ;
    constant s : string := x ;
end package ;

-------------------------------------------------------------------------------

package pack2 is
    function get_str return string;
end package ;

package body pack2 is
    package pack3 is new work.pack generic map (x => "bar") ;

    use pack3.all;

    function get_str return string is
    begin
        return pack3.s;
    end function;

end package body ;

-------------------------------------------------------------------------------

entity genpack11 is
end entity;

use work.pack2.all;

architecture test of genpack11 is
    signal s : string(1 to 3) := "bar";
begin

    p1: process is
    begin
        assert get_str = "bar";
        assert get_str = s;
        wait;
    end process;

end architecture;
