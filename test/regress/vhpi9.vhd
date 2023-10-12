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

    type r is record
        f : bit_vector(1 to k);
        g : bit_vector(k downto 1);
    end record;

end package;

-------------------------------------------------------------------------------

entity vhpi9 is
end entity;

use work.pack1.all;
use work.pack2.all;

library ieee;
use ieee.std_logic_1164.all;

architecture test of vhpi9 is
    signal s : t;
    signal s2 : std_logic_vector(k downto 0);
    signal s3 : r;
begin
end architecture;
