package pack is generic ( x : string := "foo" ) ;
    constant s : string := x ;
    constant k : integer;
end package ;

package body pack is
    constant k : integer := 42;
end package body;

-------------------------------------------------------------------------------

package pack2 is
    function get_str return string;
    constant kk : integer;
end package ;

package body pack2 is
    package pack3 is new work.pack generic map (x => "bar") ;

    use pack3.all;

    function get_str return string is
    begin
        return pack3.s;
    end function;

    constant kk : integer := pack3.k + 1;

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
        assert kk = 43;
        wait;
    end process;

end architecture;
