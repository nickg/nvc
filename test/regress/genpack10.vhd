package pack1 is
    generic (n : integer);
    type pt is protected
        procedure get_str (s : out string);
    end protected;
end package ;

package body pack1 is
    constant h : string := "hello";

    type pt is protected body
        procedure get_str (s : out string) is
        begin
            s := h;
        end procedure;
    end protected body;
end package body;

-------------------------------------------------------------------------------

package pack2 is
    procedure test;
end package;

package body pack2 is
    package inst is new work.pack1 generic map (n => 5) ;

    use inst.all;

    shared variable p : pt;

    procedure test is
        variable s : string(1 to 5);
    begin
        p.get_str(s);
        assert s = "hello";
    end procedure;

end package body ;

-------------------------------------------------------------------------------

entity genpack10 is
end entity;

use work.pack2.all;

architecture aa of genpack10 is
begin
    test;
end architecture;
