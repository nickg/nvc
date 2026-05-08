library ieee;
use ieee.std_logic_1164.all;

entity top is end entity;

architecture rtl of top is
  component veri is
    port (y : out std_logic);
  end component;
begin
  inst : veri port map (y => open);
end architecture;
