package pack1 is
    type t is (foo, bar, baz);

    function "=" (x, y : t) return boolean;
end package;

entity e is
end entity;

use work.pack1.all;

architecture a of e is
    function "=" (x, y : work.pack1.t) return boolean is
    begin
        return work.pack1."="(x, y);    -- OK (even in 93)
    end function;
begin
end architecture;
