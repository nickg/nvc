package pack1 is
    constant c1 : integer := 42;
    constant c2 : string := "hello";
end package;

-------------------------------------------------------------------------------

entity vhpi15 is
end entity;

use work.pack1.all;

architecture test of vhpi15 is
    signal s1 : integer := c1;
    signal s2 : string(1 to 5) := c2;
begin

end architecture;
