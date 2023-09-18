package pack1 is
    constant k : natural;
end package;

package body pack1 is
    constant k : natural := 4;
end package body;

-------------------------------------------------------------------------------

use work.pack1.all;

package pack2 is
    type t is array (1 to k + 1) of bit_vector(k - 1 downto 0);
end package;

-------------------------------------------------------------------------------

entity vhpi9 is
end entity;

use work.pack2.all;

architecture test of vhpi9 is
    signal s : t;
begin
end architecture;
