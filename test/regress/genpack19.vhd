package pack1 is
    function get_string return string;
end package;

package body pack1 is
    constant s : string := "hello";

    function get_string return string is
    begin
        return s;
    end function;
end package body;

-------------------------------------------------------------------------------

package pack2 is
    generic ( x : integer );
    function get_string return string;
end package;

package body pack2 is
    function get_string return string is
    begin
        return work.pack1.get_string;
    end function;
end package body;

-------------------------------------------------------------------------------

package pack3 is
    function get_string return string;
end package;

package body pack3 is
    constant s : string := "world";

    function get_string return string is
    begin
        return s;
    end function;
end package body;

-------------------------------------------------------------------------------

package pack4 is
    generic ( x : integer );
    function get_string return string;
end package;

package body pack4 is
    function get_string return string is
    begin
        return work.pack3.get_string;
    end function;
end package body;

-------------------------------------------------------------------------------

package inst1 is new work.pack2 generic map ( 4 );

-------------------------------------------------------------------------------

entity genpack19 is
end entity;

architecture test of genpack19 is
    package inst2 is new work.pack4 generic map ( 6 );
begin

    check: process is
    begin
        assert work.inst1.get_string = "hello";
        assert inst2.get_string = "world";
        wait;
    end process;

end architecture;
