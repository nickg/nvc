package p is
    pure function f(
        i : boolean
    ) return integer;
end package p;

package body p is
    pure function f(
        i : boolean
    ) return integer is
    begin
        if (i = false) then
            return 0;
        else
            return 1;
        end if;
    end function f;
end package body p;

-------------------------------------------------------------------------------

use work.p.all;

package q is
    constant d : integer := f(true) + 1;
end package q;

package body q is
end package body q;

-------------------------------------------------------------------------------

use work.q.all;

entity issue59 is
begin
    assert (d = 2);
end entity issue59;

architecture a of issue59 is
begin
end architecture a;
