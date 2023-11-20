library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity issue799 is
end entity;

architecture rtl of issue799 is
  constant C_IBC : natural := 500000; -- 0x07A120
  type t_slv_array is array (natural range <>) of std_logic_vector;
begin
  p_proc : process
    variable v_default_data  : t_slv_array(0 to 32)(15 downto 0);
  begin
    v_default_data(4) := std_logic_vector(to_unsigned(C_IBC/2, 32)(31 downto 16)) or "0000000001000000";
    assert v_default_data(4) = x"0043" report to_string(v_default_data(4)) severity failure;
    wait;
  end process;
end architecture;
