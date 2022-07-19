package pack1 is
    generic (n : integer);
    function add (x : integer) return integer;
end package ;

package body pack1 is
    function add (x : integer) return integer is
    begin
        return x + n;
    end function;
end package body;

-------------------------------------------------------------------------------

package pack2 is
    procedure test;
end package;

package body pack2 is
    package inst is new work.pack1 generic map (n => 5) ;

    use inst.all;

    procedure test is
        variable v : integer := 5;
    begin
        assert add(2) = 7;
        assert add(v) = 10;
    end procedure;

end package body ;

-------------------------------------------------------------------------------

entity genpack9 is
end entity;

use work.pack2.all;

architecture aa of genpack9 is
begin
    test;
end architecture;
