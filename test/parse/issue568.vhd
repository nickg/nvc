package pack is
    function "=" (L: bit; R: bit) return bit;
end package;

use work.pack.all;

entity issue568 is
    generic (x : boolean);
end entity;

architecture test of issue568 is
    signal y : bit;
begin
    p1: assert (y = '1') or (not x);
end architecture;
