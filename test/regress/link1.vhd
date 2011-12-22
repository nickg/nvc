package p is
    function add2(x : integer) return integer;
end package;

package body p is

    function add2(x : integer) return integer is
    begin
        return x + 2;
    end function;

end package body;

entity link1 is
end entity;

use work.p.all;

architecture test of link1 is
begin

    process is
    begin
        assert add2(5) = 7;
        wait;
    end process;

end architecture;
