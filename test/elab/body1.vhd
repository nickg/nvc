package pack is
    function get_value return integer;
end package;

-------------------------------------------------------------------------------

entity body1 is
end entity;

use work.pack.all;

architecture test of body1 is
    constant c : integer := get_value;  -- Error
begin

end architecture;
