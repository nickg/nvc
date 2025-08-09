library ieee;
use ieee.std_logic_1164.all;

entity issue1260 is
end entity issue1260;

architecture rtl of issue1260 is

  type t_slv_array is array(natural range <>) of std_logic_vector;

  function get_slv_array return t_slv_array is
    variable v_slv_array : t_slv_array(0 to 0)(7 downto 0);
  begin
    v_slv_array(0) := x"FF";
    return v_slv_array;
  end function get_slv_array;

  constant C_FAILING_ARRAY : t_slv_array(0 to 1)(7 downto 0) := get_slv_array & get_slv_array;

begin

  p_sequencer : process
  begin
    for i in 0 to 1 loop
      report to_string(C_FAILING_ARRAY(i));
    end loop;
    assert C_FAILING_ARRAY = (X"FF", X"FF");
    wait;
  end process p_sequencer;

end architecture rtl;
