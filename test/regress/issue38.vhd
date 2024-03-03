package p is
    function f (i : bit) return integer;
end package p;

package body p is
    function f (i : bit) return integer is
    begin
        report f'instance_name;
        assert f'instance_name = ":work:p:f:";
        assert f'path_name = ":work:p:f:";
        return 0;
    end function f;
end package body p;

-------------------------------------------------------------------------------

entity issue38 is
begin
end entity issue38;

use work.p.all;

architecture a of issue38 is
    function g (i : bit) return integer is
    begin
        assert g'instance_name = ":issue38(a):g:";
        assert g'path_name = ":issue38:g:";
        assert f'instance_name = ":work:p:f:" report f'instance_name;
        return 0;
    end function g;
begin
    assert (f('1') = 0);
    assert (g('1') = 0);
end architecture a;
