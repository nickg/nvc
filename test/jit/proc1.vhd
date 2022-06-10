package proc1_pack0 is
    procedure add_n (x : inout integer; n : in integer);
end package;

package body proc1_pack0 is

    procedure add_n (x : inout integer; n : in integer) is
    begin
        if x = 10 then
            wait for 1 ns;
        else
            x := x + n;
        end if;
    end procedure;

end package body;

-------------------------------------------------------------------------------

package proc1_pack1 is
    procedure add1 (x : inout integer);
end package;

use work.proc1_pack0.all;

package body proc1_pack1 is

    procedure add1 (x : inout integer) is
    begin
        add_n(x, 1);
    end procedure;

end package body;

-------------------------------------------------------------------------------

package proc1_pack2 is
    function add2 (x : integer) return integer;
end package;

use work.proc1_pack1.all;

package body proc1_pack2 is

    function add2 (x : integer) return integer is
        variable r : integer := x;
    begin
        add1(r);
        add1(r);
        return r;
    end function;

end package body;
