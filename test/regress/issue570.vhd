package pack is
    function "=" (L: bit; R: bit) return bit;
end package;

package body pack is
    function "=" (L: bit; R: bit) return bit is
    begin
        if L = R then
            return '1';
        else
            return '0';
        end if;
    end function;
end package body;

-------------------------------------------------------------------------------

use work.pack.all;

entity issue570 is
end entity;

architecture test of issue570 is
    signal x : bit := '1';
begin

    p1: process is
    begin
        assert (bit'( '1' ) = bit'( '1' )) = '1';
        if x = '0' then
            assert false;
        end if;
        wait;
    end process;

end architecture;
