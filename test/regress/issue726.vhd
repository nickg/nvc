library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package types_pkg is
  type t_slv_array is array (natural range <>) of std_logic_vector;
  subtype t_word              is std_logic_vector(15 downto 0);
  subtype t_word_array        is t_slv_array(open)(t_word'range);
  subtype t_addr              is natural range 1 to 30;
  type t_data is array (t_addr) of t_word_array(0 to 31);
end package types_pkg;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.types_pkg.all;

package protected_pkg is
  type t_config_interface is protected
    impure function get_val return t_data;
  end protected t_config_interface;
end package protected_pkg;
package body protected_pkg is
  type t_config_interface is protected body
    variable v_data : t_data := (others => (others => (others => '0')));
    impure function get_val return t_data is
    begin
      return v_data;
    end function get_val;
  end protected body t_config_interface;
end package body;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.protected_pkg.all;

package shared_variables_pkg is
  shared variable shared_config : t_config_interface;
end package shared_variables_pkg;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.types_pkg.all;
use work.shared_variables_pkg.all;

package test_pkg is
  impure function randomize(constant val : t_word_array) return t_word_array;
end package test_pkg;

package body test_pkg is
  impure function randomize(constant val : t_word_array) return t_word_array is
  begin
    return val;
  end function;
end package body;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.types_pkg.all;
use work.shared_variables_pkg.all;
use work.test_pkg.all;

entity issue726 is
end entity issue726;

architecture str of issue726 is
begin
  process
    variable v_data : t_word_array(0 to 31);
    constant C_VAL : unsigned(4 downto 0) := "00010";
  begin
    v_data := randomize(shared_config.get_val(to_integer(C_VAL)));
    for i in v_data'range loop
        assert v_data(i) = X"0000";
    end loop;
    wait;
  end process;
end architecture;
