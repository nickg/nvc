library ieee;
use     ieee.std_logic_1164.all;

entity issue1544 is
end entity;

architecture a of issue1544 is
  type ram_t is array(natural range <>) of std_logic_vector;
  signal ram : ram_t(0 to 15)(31 downto 0) := (
     0 => x"00000000",
     1 => x"01010101",
     2 => x"02020202",
     3 => x"03030303",
     4 => x"04040404",
     5 => x"05050505",
     6 => x"06060606",
     7 => x"07070707",
     8 => x"08080808",
     9 => x"09090909",
    10 => x"0A0A0A0A",
    11 => x"0B0B0B0B",
    12 => x"0C0C0C0C",
    13 => x"0D0D0D0D",
    14 => x"0E0E0E0E",
    15 => x"0F0F0F0F"
  );
begin
  assert false report "RAM: " & to_string(ram) severity note;
end architecture;
