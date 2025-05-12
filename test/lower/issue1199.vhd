entity nvc_bug is
end entity nvc_bug;

architecture rtl of nvc_bug is

  type t_slv_array is array (natural range <>) of bit_vector;

  function get_slv_array return t_slv_array is
    variable v_data_out : t_slv_array(0 to 1)(7 downto 0) := (others=>x"00");
  begin
    return v_data_out(0 to 1);
  end function get_slv_array;

  constant C_CONSTANT : t_slv_array(0 to 2)(7 downto 0)  := x"00" & get_slv_array;

begin
end architecture rtl;
