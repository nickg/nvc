package pack0 is
    constant v : bit_vector := "1010101";
end package;

-------------------------------------------------------------------------------

use work.pack0.all;

package pack1 is
    function bar return integer;
end package;

package body pack1 is

    function bar return integer is
    begin
        return 5;
    end function;

end package body;

-------------------------------------------------------------------------------

use work.pack1.all;
use work.pack0.all;

package pack2 is
    function foo return integer;
end package;

package body pack2 is

    function foo return integer is
    begin
        return bar + 2;
    end function;

end package body;

-------------------------------------------------------------------------------

entity link2 is
end entity;

use work.pack2.all;

architecture test of link2 is
begin

    process is
    begin
        assert foo = 7;
        wait;
    end process;

end architecture;
