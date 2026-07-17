package pack is
    type pt is protected
    end protected;
end package;

use work.pack.all;

entity e is
  port (variable s : inout pt);         -- Error
end e;

architecture a of e is
begin
end a;
