package pack is
    constant hello : string := "hello, world";
    alias a is hello(1 to 3);
end package;

-------------------------------------------------------------------------------

entity alias1 is
end entity;

use work.pack.all;

architecture test of alias1 is
begin
    p1: assert false report a;
end architecture;
