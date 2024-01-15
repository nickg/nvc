package pack is
    function get_value return integer;
end package;

package body pack is
    function get_value return integer is
    begin
        return 5;
    end function;
end package body;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub is
    generic ( G : integer := get_value );
    port ( x : bit_vector(1 to G) );
end entity;

architecture test of sub is
begin
end architecture;

-------------------------------------------------------------------------------

entity bounds11 is
end entity;

architecture test of bounds11 is
    signal s : bit_vector(1 to 3);
begin

    u: entity work.sub
        port map ( s );                 -- Error

end architecture;
