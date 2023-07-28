library ieee;
use ieee.math_real.all;

package pack is
    function clog2(x: positive) return natural;
end package;

package body pack is
    function clog2(x: positive) return natural is begin
       return natural(ceil(log2(real(x))));
    end;
end package body;

use work.pack.all;

entity issue744 is end;

architecture test of issue744 is
    subtype u is bit_vector(clog2(50) - 1 downto 0);
    signal b : u;
begin
end architecture;
