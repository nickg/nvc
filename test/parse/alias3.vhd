package pack1 is
    type t is (foo, bar);
end package;

-------------------------------------------------------------------------------

use work.pack1.all;

package pack2 is
    alias foo is work.pack1.foo [return t];
    procedure foo;
    procedure bar(arg : t);
end package;

-------------------------------------------------------------------------------

entity e is
end entity;

use work.pack2.all;

architecture test of e is
begin
    p1: process is
    begin
        bar(foo);                       -- OK
        wait;
    end process;
end architecture;
