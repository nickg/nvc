package pack is
    function expensive (x : string) return integer;
end package;

package body pack is
    function expensive (x : string) return integer is
    begin
        return integer'value(x);
    end function;
end package body;

-------------------------------------------------------------------------------

use work.pack.all;

entity issue837 is
end entity;

architecture test of issue837 is
    signal x : bit_vector(1 to 12);
    constant str : string := "12";
begin
    b: block is
        port ( p : in bit_vector(1 to expensive(str)) );
        port map ( p => x );
        signal s : p'subtype;               -- Should not call EXPENSIVE
    begin
    end block;

end architecture;
