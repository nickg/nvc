library ieee;
use ieee.std_logic_1164.all;

package test_pkg is
  signal glob_sig : std_logic := '1';
end package;

library ieee;
use ieee.std_logic_1164.all;
use work.test_pkg.all;

entity issue483 is
end entity;

architecture rtl of issue483 is
begin
  p_proc : process
    constant C_CONST : std_logic := glob_sig;
  begin
    assert C_CONST = '1';
    wait;
  end process;
end architecture;
