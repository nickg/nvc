package pack is
    function get_bits (n : natural) return bit_vector;
end package;

package body pack is
    function get_bits (n : natural) return bit_vector is
        variable result : bit_vector(1 to n);
    begin
        return result;
    end function;
end package body;

-------------------------------------------------------------------------------

entity sub is
    port (p, q : bit_vector);
end entity;

architecture test of sub is
begin
end architecture;

-------------------------------------------------------------------------------

entity issue613 is
end entity;

use work.pack.all;

architecture test of issue613 is
begin

    u: entity work.sub
        port map (
            p(1) => '0',
            q => get_bits(5) );

end architecture;
