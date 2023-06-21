package types_pkg is
  type t_slv_array is array (natural range <>) of bit_vector;
  subtype t_word              is bit_vector(15 downto 0);
  subtype t_word_array        is t_slv_array(open)(t_word'range);
  subtype t_addr              is natural range 1 to 30;
  type t_data is array (t_addr) of t_word_array(0 to 31);
end package types_pkg;

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

use work.protected_pkg.all;

package shared_variables_pkg is
  shared variable shared_config : t_config_interface;
end package shared_variables_pkg;

use work.types_pkg.all;
use work.shared_variables_pkg.all;

package test_pkg is
  impure function randomize(constant val : t_word_array) return t_word_array;
  impure function randomize(constant sub_address : bit_vector) return t_word_array;
end package test_pkg;

package body test_pkg is
  impure function randomize(constant val : t_word_array) return t_word_array is
  begin
    return val;
  end function;
  impure function randomize(constant sub_address : bit_vector)
    return t_word_array is
    variable v_test : t_word_array(0 to 31);
  begin
    v_test := shared_config.get_val(14);  -- OK
    v_test := randomize(v_test);        -- OK
    return randomize(shared_config.get_val(12));  -- OK
  end function randomize;
end package body;
