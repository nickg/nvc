library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package test is
  type t_memory_map_array is array (natural range 0 to 10) of natural;
  constant C_VER : unsigned(4 downto 0) := "00001";
  constant C_MEMORY_MAP_ITEM_DEFAULT : natural := 0;
  constant C_MEMORY_MAP_DEFAULT : t_memory_map_array := (
    to_integer(C_VER) => 1,
    others => C_MEMORY_MAP_ITEM_DEFAULT);
end package;
