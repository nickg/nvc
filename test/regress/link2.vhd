package pack0 is
    constant foo : integer := 42;
    constant v : bit_vector := "1010101";
    constant q : bit_vector(3 downto 0) := X"F";
    constant z : integer := 2;
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
    function get_v return bit_vector;
    function get_v0 return bit;
end package;

package body pack2 is

    constant x : bit_vector(1 downto 0) := q(1 downto 0);
    constant gv : bit_vector(6 downto 0) := get_v;

    function foo return integer is
    begin
        return bar + 2;
    end function;

    function get_v return bit_vector is
    begin
        return v;
    end function;

    function get_v0 return bit is
    begin
        return v(0);
    end function;

end package body;

-------------------------------------------------------------------------------

entity link2 is
end entity;

use work.pack2.all;

architecture test of link2 is
begin

    process is
        variable v : bit_vector(6 downto 0);
    begin
        assert foo = 7;
        assert get_v = "1010101";       -- Will be constant folded
        v := get_v;
        wait for 1 ns;
        assert v = get_v;
        assert get_v0 = '1';
        wait;
    end process;

end architecture;
