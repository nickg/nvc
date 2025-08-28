entity issue1277 is
end entity;

architecture rtl of issue1277 is

  type t_slv_array is array(natural range <>) of bit_vector;

  function get_slv_array return t_slv_array is
    variable v_slv_array : t_slv_array(0 to 0)(7 downto 0);
  begin
    v_slv_array(0) := x"FF";
    return v_slv_array;
  end function get_slv_array;

  constant C_ZERO_ARRAY_ELEMENT : t_slv_array(0 to 0)(7 downto 0) := (0 => x"00");
  constant C_FAILING_ARRAY      : t_slv_array(0 to 1)(7 downto 0) := C_ZERO_ARRAY_ELEMENT & get_slv_array;

begin

    process is
    begin
        assert C_FAILING_ARRAY = (x"00", x"ff");
        wait;
    end process;

end architecture rtl;
