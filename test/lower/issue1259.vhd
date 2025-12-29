entity issue1259 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of issue1259 is
    function func (a, b : std_ulogic_vector(1 downto 0)) return std_ulogic_vector is
        variable res : std_ulogic_vector(1 downto 0);
    begin
        res(0) := ((a(1)) and b(0)) xor
                  ((a(1)) and b(1)) xor
                  ((a(0)) and b(1));
        res(1) := ((a(1)) and b(1)) xor
                  ((a(0)) and b(0)) xor
                  ((a(0)) and b(1));
        return res;
    end function;
begin
end architecture;
