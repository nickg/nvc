package pack is
    generic (type t; function "+" (x, y : t) return t is <>);
    function add(x, y : t) return t;
end package;

package body pack is
    function add(x, y : t) return t is
    begin
        return x + y;
    end function;
end package body;

-------------------------------------------------------------------------------

entity issue1266 is
    package pack_int is new work.pack generic map (integer);
end entity;

architecture test of issue1266 is
    use pack_int.all;
begin
    assert add(3, 5) = 8;
end architecture;
