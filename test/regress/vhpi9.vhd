package pack is
    constant k : natural;
    type t is array (1 to k + 1) of bit_vector(k - 1 downto 0);
end package;

package body pack is
    constant k : natural := 4;
end package body;

-------------------------------------------------------------------------------

entity vhpi9 is
end entity;

use work.pack.all;

architecture test of vhpi9 is
    signal s : t;
begin
end architecture;
