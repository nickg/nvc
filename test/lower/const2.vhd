package const2pack is
    constant width : integer;
end package;

package body const2pack is
    constant width : integer := 5;
end package body;

entity const2 is
end entity;

use work.const2pack.all;

architecture test of const2 is
    signal s : bit_vector(1 to width);  -- OK
begin
end architecture;
