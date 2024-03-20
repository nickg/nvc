package pkg is
    function dummy_function return natural;
end package pkg;

package body pkg is
    function dummy_function return natural is
    begin
        return 4;
    end function;
end package body pkg;

library work;
use work.pkg.all;

entity ent is
    -- Should get rewritten before signals below
    constant CONST : natural  := dummy_function;
end entity ent;

architecture arch of ent is
    signal s0 : bit;
    signal s1 : bit_vector(CONST downto 0);
    signal s2 : bit_vector(CONST downto 0);
begin
    s2 <= s1 and (s1'range => s0);
end architecture arch;
